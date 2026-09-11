"""FE3 hardware mapping regressions using the actual assembled cartridge.

Run: make test-fe3 PYTHON=myenv/bin/python
"""
from pathlib import Path
from collections import deque
import unittest
import os
from functools import cache
from py65.devices.mpu6502 import MPU

# Snapshot a matching build once: another terminal may rebuild while tests run.
@cache
def cartridge_build():
    return Path('monster-fe3.bin').read_bytes(), Path('labels.txt').read_text().splitlines()

class Memory:
    def __init__(self, image):
        self.rom = image
        self.ram = bytearray(b'\xff\x00' * (256 * 1024))
        self.base = bytearray(65536)
        roms = Path(os.environ.get('VICE_ROM_DIR', '/opt/homebrew/share/vice/VIC20'))
        self.base[0xc000:0xe000] = (roms / 'basic-901486-01.bin').read_bytes()
        self.base[0xe000:] = (roms / 'kernal.901486-07.bin').read_bytes()
        self.reg = 0
        self.config = 0
        self.locked = True

    def location(self, address, write=False):
        if 0xa000 <= address < 0xc000:
            self.locked = not write
        if 0x400 <= address < 0x1000:
            if self.config & 1:
                return self.base, address
            return self.ram, address
        for bit, start, offset in ((2, 0x2000, 0), (4, 0x4000, 0x2000),
                                   (8, 0x6000, 0x4000), (16, 0xa000, 0x6000)):
            if start <= address < start + 8192:
                if self.config & bit:
                    return self.base, address
                mode = self.reg & 0xe0
                if mode == 0:
                    if bit != 16:
                        return self.base, address
                bank = self.reg & 15
                if mode in (0, 0x40) and write:
                    bank = 1  # Super ROM writes always target RAM 1.
                data = self.rom if not write and mode in (0, 0x40) else self.ram
                return data, bank * 32768 + offset + address - start
        return self.base, address

    def __getitem__(self, address):
        if isinstance(address, slice):
            return [self[i] for i in range(address.start, address.stop)]
        if 0x9c00 <= address < 0xa000:
            return self.reg if address & 3 == 2 else self.config if address & 3 == 3 else 0
        data, offset = self.location(address)
        return data[offset]

    def __setitem__(self, address, value):
        if 0x9c00 <= address < 0xa000:
            if not self.config & 0x80 and (self.reg & 0xe0 or not self.locked):
                if address & 3 == 2:
                    self.reg = value
                elif address & 3 == 3:
                    self.config = value
            return
        data, offset = self.location(address, True)
        if data is not self.rom and address < 0xc000:
            data[offset] = value


class Machine:
    def __init__(self, image=None, labels=None):
        if image is None and labels is None:
            image, labels = cartridge_build()
        self.symbols = {}
        # Exported symbols precede potentially same-named local debug labels.
        for line in labels:
            _, address, name = line.split()
            self.symbols.setdefault(name.lstrip('.'), int(address, 16))
        self.mem = Memory(image)
        self.cpu = MPU(memory=self.mem)
        self.history = deque(maxlen=25)
        self.hooks = set()
        self.io_hooks = {}

    def run_until(self, address, steps=3_000_000):
        for _ in range(steps):
            if self.cpu.pc == address:
                return
            self.history.append((hex(self.cpu.pc), hex(self.mem.reg)))
            if self.cpu.pc in self.io_hooks:
                self.io_hooks[self.cpu.pc]()
                self.cpu.pc = self.cpu.stPopWord() + 1
                continue
            if self.mem.reg == 0xa1 and self.cpu.pc in self.hooks:
                self.cpu.pc = self.cpu.stPopWord() + 1
                continue
            if self.mem[self.cpu.pc] in (0x02, 0x12, 0x22, 0x32, 0x42, 0x52,
                                         0x62, 0x72, 0x92, 0xb2, 0xd2, 0xf2):
                raise AssertionError(f'CPU JAM at ${self.cpu.pc:04x}, bank=${self.mem.reg:02x}, history={list(self.history)}')
            self.cpu.step()
        raise AssertionError(f'PC=${self.cpu.pc:04x}, bank=${self.mem.reg:02x}, history={list(self.history)}')

    def boot(self):
        self.mem.reg = self.mem.config = 0
        self.mem.locked = True
        self.cpu.pc = self.mem[0xa000] | self.mem[0xa001] << 8
        self.run_until(self.symbols['enter'])

    def call(self, name, a=0, xy=0, bank=0xa1):
        self.mem.reg = bank
        self.cpu.a, self.cpu.x, self.cpu.y = a, xy & 255, xy >> 8
        self.cpu.stPushWord(0x3ef)
        self.cpu.pc = self.symbols[name]
        self.run_until(0x3f0)
        return self.cpu.a, self.cpu.x | self.cpu.y << 8, bool(self.cpu.p & 1)


class FE3(unittest.TestCase):
    def machine(self):
        m = Machine()
        m.boot()
        m.cpu.step()
        m.hooks = {m.symbols[n] for n in ('__irq_on', '__irq_off', '__file_init_drive', '__screen_blank', '__screen_unblank')}
        m.run_until(m.symbols['__edit_run'])
        return m

    def test_boot_relocations(self):
        m = Machine()
        m.boot()
        self.assertEqual(m.mem.reg, 0xa1)
        for segment in ('BANKCODE', 'BANKCODE2', 'IRQ', 'DATA', 'FE3CFG'):
            start = m.symbols[f'__{segment}_LOAD__']
            run = m.symbols[f'__{segment}_RUN__']
            size = m.symbols[f'__{segment}_SIZE__']
            self.assertEqual(bytes(m.mem[run:run+size]), m.mem.rom[start-0xa000+0x6000:start-0xa000+0x6000+size], segment)

    def test_enter(self):
        m = self.machine()
        self.assertEqual(m.mem.reg, 0xa1)

    def test_virtual_memory(self):
        m = self.machine()
        for address in (0, 0x3ff, 0x400, 0xfff, 0x1000, 0x1fff,
                        0x2000, 0x3fff, 0x4000, 0x6000, 0x7fff,
                        0x9000, 0x9110, 0x9400, 0x97ff, 0xa000, 0xbfff):
            with self.subTest(address=hex(address)):
                m.call('__vmem_store', a=0x5a, xy=address)
                result = m.call('__vmem_load', xy=address)
                self.assertEqual(result[:2], (0x5a, address))
                self.assertEqual(m.mem.reg, 0xa1)

    def test_assembly(self):
        m = self.machine()
        source = ['.org $2000', '.eq VALUE 7', 'MAIN lda #VALUE',
                  'sta $900f', 'bne MAIN', 'rts']
        m.call('__asm_reset')
        linebuf = m.symbols['__linebuffer']
        for pass_number in (1, 2):
            m.call('__asm_startpass', a=pass_number)
            for number, line in enumerate(source, 1):
                m.mem[m.symbols['__asm_linenum']] = number
                for i, byte in enumerate(line.encode() + b'\0'):
                    m.mem[linebuf+i] = byte
                result = m.call('__asm_tokenize', a=0xa1, xy=linebuf)
                self.assertFalse(result[2], (line, result))
        result = bytes(m.call('__vmem_load', xy=0x2000+i)[0] for i in range(8))
        self.assertEqual(result, bytes.fromhex('a9 07 8d 0f 90 d0 f9 60'))

    def test_native_round_trip(self):
        for blocks in (0x1f, 0, 1, 2, 6, 0x0e, 0x10):
            with self.subTest(blocks=blocks):
                m = self.machine()
                m.mem[m.symbols['__memcfg_blocks']] = blocks
                m.call('__memcfg_apply')
                program = bytes.fromhex('a9 5a 8d 00 04 8d ff 0f a9 67 85 20 00')
                for i, value in enumerate(program):
                    m.call('__vmem_store', a=value, xy=0x1200+i)
                m.mem[m.symbols['__sim_pc']] = 0
                m.mem[m.symbols['__sim_pc']+1] = 0x12
                m.mem[m.symbols['__sim_reg_sp']] = 0xf0
                m.mem[m.symbols['__sim_reg_p']] = 0x24
                m.cpu.pc = m.symbols['__run_go']
                m.run_until(0x1200)
                self.assertEqual(m.mem.reg, 0xaa)
                self.assertEqual(m.mem.config, (blocks | 8) ^ 0x1f)
                m.run_until(m.symbols['return_to_debugger'])
                self.assertEqual(m.mem.reg, 0xa1)
                self.assertEqual(m.mem.config, 0)
                self.assertEqual(m.call('__vmem_load', xy=0x20)[0], 0x67)
                if blocks & 1:
                    self.assertEqual(m.call('__vmem_load', xy=0x400)[0], 0x5a)
                    self.assertEqual(m.call('__vmem_load', xy=0xfff)[0], 0x5a)

    def test_source_banks_and_reset(self):
        m = self.machine()
        for number in range(4):
            if number:
                self.assertFalse(m.call('__src_new')[2])
            self.assertEqual(m.mem[m.symbols['__src_bank']], 0xac + number)
            for _ in range(300):
                self.assertFalse(m.call('__src_insert', a=ord('A') + number)[2])
            self.assertEqual(m.mem.ram[(12+number)*32768:(12+number)*32768+300],
                             bytes([ord('A')+number])*300)
        self.assertTrue(m.call('__src_new')[2])
        self.assertEqual(m.mem[m.symbols['__src_numbuffers']], 4)
        self.assertFalse(m.call('__src_new_log')[2])
        m.call('__src_force_set', a=4)
        self.assertEqual(m.mem[m.symbols['__src_bank']], 0xab)
        m.call('__src_set', a=0)
        before = bytes(m.mem.ram[12*32768:16*32768])
        m.boot()
        self.assertEqual(bytes(m.mem.ram[12*32768:16*32768]), before)
        self.assertEqual(m.mem[m.symbols['__src_numbuffers']], 4)

    def test_scroll(self):
        m = self.machine()
        original = bytes((i // 8 + i % 8 * 17) & 255 for i in range(3840))
        for down in (False, True):
            for first, last, amount in ((0, 23, 1), (1, 21, 4), (4, 10, 0), (4, 10, 8)):
                for i, byte in enumerate(original):
                    m.mem[0x1100+i] = byte
                expected = bytearray(original)
                count = max(0, last-first+1-amount)*8
                if amount:
                    for column in range(20):
                        start = column*192 + first*8
                        src, dst = (start, start+amount*8) if down else (start+amount*8, start)
                        expected[dst:dst+count] = original[src:src+count]
                a, x = (first, last) if down else (last, first)
                m.call('__text_scrolldownn' if down else '__text_scrollupn', a=a, xy=x | amount << 8)
                self.assertEqual(bytes(m.mem[0x1100:0x2000]), expected)

    def test_native_restore_key(self):
        m = self.machine()
        # The cassette buffer belongs to the user, not the transition code.
        # Also overwrite both ends of the shared RAM123 region before NMI.
        for i, byte in enumerate(bytes.fromhex('a9 5a 8d 3c 03 8d ff 03 8d 00 04 8d ff 0f 4c 0e 12')):
            m.call('__vmem_store', a=byte, xy=0x1200+i)
        cassette = bytes([0x5a] + list(range(0xa1, 0xb0)))
        for i, byte in enumerate(cassette):
            m.call('__vmem_store', a=byte, xy=0x33c+i)
        for attempt in range(2):
            with self.subTest(attempt=attempt):
                m.mem[m.symbols['__sim_pc']] = 0
                m.mem[m.symbols['__sim_pc']+1] = 0x12
                m.mem[m.symbols['__sim_reg_sp']] = 0xf0
                vectors = bytes(m.mem[0x316:0x31a])
                bank_tops = [bytes(m.mem.ram[bank*32768+0x5f80:bank*32768+0x6000])
                             for bank in range(16)]
                m.cpu.pc = m.symbols['__run_go']
                m.run_until(0x120e)
                self.assertEqual(m.mem[0x318] | m.mem[0x319] << 8, 0x7fe0)
                self.assertEqual(m.mem[0x316] | m.mem[0x317] << 8, 0x7fe5)
                for bank in range(16):
                    if bank not in (1, 10):
                        self.assertEqual(bytes(m.mem.ram[bank*32768+0x5f80:bank*32768+0x6000]),
                                         bank_tops[bank], f'bank {bank}')
                self.assertEqual(bytes(m.mem[0x7f80:0x7fe0]), bank_tops[10][:0x60])
                self.assertEqual(bytes(m.mem[0x33c:0x34c]), cassette)
                m.cpu.nmi()
                m.run_until(0x7fe0)
                # The first bank write continues immediately in MAIN; no
                # handler or trampoline is installed in SIM or other banks.
                m.run_until(0x7fef)
                self.assertEqual(m.mem.reg, 0xa1)
                self.assertEqual(m.mem[m.cpu.pc], 0x4c)  # JMP native_enter
                m.run_until(m.symbols['return_to_debugger'])
                self.assertEqual(m.mem.reg, 0xa1)
                self.assertEqual(m.mem.config, 0)
                self.assertEqual(m.mem[m.symbols['__sim_pc']], 0x0e)
                self.assertEqual(m.mem[m.symbols['__sim_pc']+1], 0x12)
                self.assertEqual(bytes(m.mem[0x316:0x31a]), vectors)
                self.assertEqual(bytes(m.call('__vmem_load', xy=0x33c+i)[0]
                                       for i in range(16)), cassette)
                for address in (0x33c, 0x3ff, 0x400, 0xfff):
                    self.assertEqual(m.call('__vmem_load', xy=address)[0], 0x5a)

    def test_basic_restore_key(self):
        m = self.machine()
        vectors = bytes(m.mem[0x316:0x31a])
        m.mem[m.symbols['__debug_interface']] = 0
        m.cpu.pc = m.symbols['__run_go_basic']
        m.run_until(0xc474)
        self.assertEqual(m.mem.reg, 0xaa)
        self.assertEqual(m.mem[0x318] | m.mem[0x319] << 8, 0x7fe0)
        m.cpu.nmi()
        m.run_until(m.symbols['__edit_run'])
        self.assertEqual(m.mem.reg, 0xa1)
        self.assertEqual(bytes(m.mem[0x316:0x31a]), vectors)

    def test_shared_signal_handlers(self):
        m = self.machine()
        # The monitor's RESTORE handler must work before any native GO has
        # installed the USER handler, even when SIM is currently mapped.
        m.cpu.pc = m.symbols['install_nmi']
        m.cpu.stPushWord(0x3ef)
        m.mem.reg = 0xa0
        m.run_until(0x3f0)
        monitor_nmi = m.mem[0x318] | m.mem[0x319] << 8
        self.assertTrue(0x400 <= monitor_nmi < 0x1000)
        m.mem.reg = 0xa9
        m.cpu.pc = 0x3f0
        m.cpu.nmi()
        m.run_until(0x3f0)
        self.assertEqual(m.mem.reg, 0xa9)
        m.mem.reg = 0xa0
        self.assertEqual(m.mem[m.symbols['__monitor_int']], 1)
        m.call('install_trace_nmi')
        trace_nmi = m.mem[0x318] | m.mem[0x319] << 8
        self.assertTrue(0x400 <= trace_nmi < 0x1000)
        m.mem.reg = 0xa9
        m.cpu.pc = 0x3f0
        m.cpu.nmi()
        m.run_until(0x3f0)
        self.assertEqual(m.mem.reg, 0xa9)
        self.assertEqual(m.mem[m.symbols['stop_tracing']], 1)
        m.call('uninstall_trace_nmi')
        self.assertEqual(m.mem[0x318] | m.mem[0x319] << 8, monitor_nmi)

    def test_control_registers_are_protected(self):
        m = self.machine()
        for address in (0x9800, 0x9c02, 0x9c03, 0x9ffe):
            m.call('__vmem_store', a=0, xy=address)
            self.assertEqual(m.mem.reg, 0xa1)
            self.assertEqual(m.mem.config, 0)
        for address in (0x7fe0, 0x7fff, 0x8000, 0x9c02, 0xc000):
            self.assertTrue(m.call('__vmem_writable', xy=address)[2])
        for address in (0x20, 0x33c, 0x3ff, 0x400, 0x7f7f, 0x7f80, 0x7fdf, 0xa000, 0xbfff):
            self.assertFalse(m.call('__vmem_writable', xy=address)[2])

    def test_full_size_link_file(self):
        m = self.machine()
        data = (b' ' * 600 + b'MEMORY [\rA:\rSTART=$2000\rEND=$4000;\r]\r'
                b'SEGMENTS [\rCODE:\rLOAD=A;\r]\r')
        position = 0

        def ok():
            m.cpu.a = 2
            m.cpu.p &= ~1

        def get():
            nonlocal position
            m.cpu.a = data[position] if position < len(data) else 0
            position += 1
            m.cpu.p = m.cpu.p & ~2 | (2 if m.cpu.a == 0 else 0)

        def status():
            m.cpu.a = 0x40 if position >= len(data) else 0

        m.io_hooks = {m.symbols['__file_open_r']: ok, m.symbols['__file_close']: ok,
                      0xffc6: ok, 0xffcf: get, 0xffb7: status}
        m.call('__link_init', bank=0xa5)
        result = m.call('__link_parse', bank=0xa5)
        self.assertFalse(result[2], result)

    def test_editor_assembly_completion(self):
        m = self.machine()
        linebuf = m.symbols['__linebuffer']
        for i, byte in enumerate(b'test.s\0'):
            m.mem[linebuf+i] = byte
        m.call('__src_name', xy=linebuf)
        for byte in b'.org $2000\rrts\r':
            m.call('__src_insert', a=byte)

        def key():
            m.cpu.a = 13
            m.cpu.p &= ~1

        # Skip actual disk saving and dismiss the completion modal, but run
        # its rendering and all summary/log/error callbacks without stubs.
        m.io_hooks = {m.symbols[n]: key for n in
                      ('prompt_saveall', '__key_flush', '__key_waitch')}
        result = m.call('command_asmdbg')
        self.assertFalse(result[2], result)
        self.assertEqual(m.call('__vmem_load', xy=0x2000)[0], 0x60)
        self.assertEqual(m.mem[m.symbols['__src_activebuff']], 0)
        self.assertEqual(m.mem.reg, 0xa1)

    def test_symbol_viewer_formats(self):
        cases = ((Path('tests/mac.s').read_bytes(), None),
                 (b'.org $2000\n.eq AZP 7\n.eq BABS $1234\n.eq CFLOAT .5\n',
                  ((b'$07', b'AZP'), (b'$1234', b'BABS'), (b'CFLOAT = .5',))))
        for source, expected_rows in cases:
            with self.subTest(source=source[:40]):
                m = self.machine()
                linebuf = m.symbols['__linebuffer']
                for i, byte in enumerate(b'symbols.s\0'):
                    m.mem[linebuf+i] = byte
                m.call('__src_name', xy=linebuf)
                for byte in source.replace(b'\n', b'\r'):
                    self.assertFalse(m.call('__src_insert', a=byte)[2])

                def ok():
                    m.cpu.a = 13
                    m.cpu.p &= ~1

                m.io_hooks = {m.symbols[n]: ok for n in
                              ('prompt_saveall', '__key_flush', '__key_waitch')}
                self.assertFalse(m.call('command_asmdbg')[2])
                self.assertEqual(m.mem[m.symbols['__label_num']], 3)
                stack = m.cpu.sp
                if expected_rows:
                    m.mem[0xe4] = 0  # viewer's sortby (zp::tmp14): alphabetical
                    # Render alternating byte, word, and float values through
                    # EXPR, including a second pass after the format changed.
                    for index in (0, 1, 2, 0, 1):
                        m.call('get_item_impl', xy=index, bank=0xa3)
                        m.call('print_item_impl', a=0, bank=0xa3)
                        start = m.symbols['__linebuffer2']
                        row = bytes(m.mem[start:start+40])
                        for text in expected_rows[index]:
                            self.assertIn(text, row)
                        self.assertEqual(m.cpu.sp, stack)
                        self.assertEqual(m.mem.reg, 0xa3)

                keys = iter((0x85, 0x85, 3))  # both sort orders, then quit

                def key():
                    m.cpu.a = next(keys)
                    m.cpu.p &= ~1

                m.io_hooks[m.symbols['__key_waitch']] = key
                m.call('__symview_enter')
                self.assertEqual(m.cpu.sp, stack)
                self.assertEqual(m.mem.reg, 0xa1)
                self.assertEqual(list(keys), [])

    def test_monitor_messages_cross_banks(self):
        m = self.machine()
        output = bytearray()

        def ok():
            m.cpu.p &= ~1

        def write():
            output.append(m.cpu.a)
            ok()

        def disassembly_error():
            m.cpu.p |= 1

        m.io_hooks = {0xffc9: ok, 0xffd2: write,
                      m.symbols['__asm_disassemble']: disassembly_error}
        m.mem.reg = 0xa0
        m.mem[m.symbols['__monitor_outfile']] = 2
        m.mem[m.symbols['__debug_interface']] = 1
        stack = m.cpu.sp
        m.call('print_tracing')
        self.assertIn(b'tracing', output.lower())
        self.assertEqual(m.cpu.sp, stack)

        output.clear()
        m.call('put_instruction', bank=0xa0)
        self.assertEqual(output, b'???\r')
        self.assertEqual(m.cpu.sp, stack)

        output.clear()
        linebuf = m.symbols['__linebuffer']
        for i, byte in enumerate(b'>r > \0'):
            m.mem[linebuf+i] = byte
        self.assertTrue(m.call('set___monitor_outfile', bank=0xa0)[2])
        m.mem.reg = 0xa1
        start = m.symbols['__str_no_file']
        expected = bytes(m.mem[start:start+40]).split(b'\0')[0]
        self.assertEqual(output, expected + b'\r')
        self.assertEqual(m.cpu.sp, stack)

    def test_simulator_native_handler_boundary(self):
        m = self.machine()
        m.call('__sim_init')
        for address in (0x7f80, 0x7fdf, 0x7fe0, 0x7fff):
            with self.subTest(address=hex(address)):
                for i, byte in enumerate((0x8d, address & 255, address >> 8)):
                    m.call('__vmem_store', a=byte, xy=0x2000+i)
                m.call('__vmem_store', a=0x11, xy=address)
                m.mem[m.symbols['__sim_pc']] = 0
                m.mem[m.symbols['__sim_pc']+1] = 0x20
                m.mem[m.symbols['__sim_reg_a']] = 0x5a
                result = m.call('__sim_step')
                protected = address >= 0x7fe0
                self.assertEqual(result[2], protected)
                self.assertEqual(m.mem[m.symbols['__sim_vital_addr_clobbered']],
                                 int(protected))
                self.assertEqual(m.call('__vmem_load', xy=address)[0],
                                 0x11 if protected else 0x5a)

    def test_trace_visible_and_banked_memory(self):
        m = self.machine()
        m.call('__sim_init')
        program = bytes.fromhex('ea a9 5a 8d 00 10 8d 00 30 a9 1b 8d 0f 90 00')
        for i, byte in enumerate(program):
            m.call('__vmem_store', a=byte, xy=0x2000+i)
        m.mem[m.symbols['__sim_pc']] = 0
        m.mem[m.symbols['__sim_pc']+1] = 0x20
        m.mem[m.symbols['__sim_reg_p']] = 0x24
        m.call('__debug_trace')
        self.assertEqual(m.mem.reg, 0xa1)
        self.assertEqual(m.call('__vmem_load', xy=0x1000)[0], 0x5a)
        self.assertEqual(m.call('__vmem_load', xy=0x3000)[0], 0x5a)
        self.assertEqual(m.call('__vmem_load', xy=0x900f)[0], 0x1b)

    def test_trace_load_flags(self):
        m = self.machine()
        cases = [(0x2000, opcode, address)
                 for opcode in (0xad, 0xae, 0xac, 0xaf)  # LDA/LDX/LDY/LAX
                 for address in (0x1000, 0x900f, 0x9400)]
        cases += [(0x1200, opcode, None) for opcode in (0xa9, 0xa2, 0xa0)]
        for start, opcode, address in cases:
            for value in (0, 0x80, 0x7f):
                with self.subTest(start=hex(start), opcode=hex(opcode),
                                  address=address, value=value):
                    m.call('__sim_init')
                    if address is None:
                        instruction = bytes((opcode, value))
                    else:
                        m.call('__vmem_store', a=value, xy=address)
                        instruction = bytes((opcode, address & 255, address >> 8))
                    # TRACE single-steps its first instruction before swapping
                    # the display in. Capture P after the following traced load.
                    program = b'\xea' + instruction + bytes.fromhex('08 68 8d 00 30 00')
                    for i, byte in enumerate(program):
                        m.call('__vmem_store', a=byte, xy=start+i)
                    m.mem[m.symbols['__sim_pc']] = start & 255
                    m.mem[m.symbols['__sim_pc']+1] = start >> 8
                    m.mem[m.symbols['__sim_reg_p']] = 0x24
                    m.call('__debug_trace')
                    flags = m.call('__vmem_load', xy=0x3000)[0]
                    self.assertEqual(flags & 0x82, 2 if value == 0 else value & 0x80)
                    self.assertEqual(m.mem.reg, 0xa1)


class FE3CartridgeRecovery(unittest.TestCase):
    def test_reset_during_native_execution(self):
        for basic in (False, True):
            with self.subTest(basic=basic):
                m = FE3().machine()
                contents = (b'first buffer\rsecond line\r', b'other unsaved buffer\r')
                names = (b'first.s', b'second.s')
                linebuf = m.symbols['__linebuffer']
                for number, content in enumerate(contents):
                    if number:
                        m.call('__src_new')
                    for i, byte in enumerate(names[number] + b'\0'):
                        m.mem[linebuf+i] = byte
                    m.call('__src_name', xy=linebuf)
                    for byte in content:
                        m.call('__src_insert', a=byte)
                # Leave the active buffer unsaved: its current gap pointers
                # still live in zero page, not in the per-buffer state table.
                source = bytes(m.mem.ram[12*32768:16*32768])
                for i, byte in enumerate(bytes.fromhex('4c 00 12')):
                    m.call('__vmem_store', a=byte, xy=0x1200+i)
                m.mem[m.symbols['__sim_pc']] = 0
                m.mem[m.symbols['__sim_pc']+1] = 0x12
                m.cpu.pc = m.symbols['__run_go_basic' if basic else '__run_go']
                m.run_until(0x1200)
                # Native code can own every byte of RAM123, including the
                # usual recovery signature and source-buffer bookkeeping.
                for address in range(0x400, 0x1000):
                    m.mem[address] = 0x5a
                m.boot()
                self.assertEqual(bytes(m.mem.ram[12*32768:16*32768]), source)
                self.assertEqual(m.mem[m.symbols['__src_numbuffers']], 2)
                prompts = []

                def recover():
                    prompts.append(True)
                    m.cpu.a = ord('Y')

                m.io_hooks = {m.symbols['__key_waitch']: recover,
                              m.symbols['__key_flush']: lambda: None}
                m.cpu.step()
                m.run_until(m.symbols['__edit_run'])
                self.assertEqual(prompts, [True])
                for number, content in enumerate(contents):
                    m.call('__src_set', a=number)
                    _, address, _ = m.call('__src_get_filename', a=number)
                    self.assertEqual(bytes(m.mem[address:address+len(names[number])]),
                                     names[number])
                    m.call('__src_rewind')
                    for line in content.split(b'\r')[:-1]:
                        m.call('__src_get')
                        self.assertEqual(bytes(m.mem[linebuf:linebuf+len(line)]), line)
                        m.call('__src_down')

    def test_reset_after_normal_native_return(self):
        for brk in (False, True):
            with self.subTest(brk=brk):
                m = FE3().machine()
                program = b'\x00' if brk else bytes.fromhex('4c 00 12')
                for i, byte in enumerate(program):
                    m.call('__vmem_store', a=byte, xy=0x1200+i)
                m.mem[m.symbols['__sim_pc']] = 0
                m.mem[m.symbols['__sim_pc']+1] = 0x12
                m.cpu.pc = m.symbols['__run_go']
                m.run_until(0x1200)
                if not brk:
                    m.cpu.nmi()
                m.run_until(m.symbols['return_to_debugger'])
                m.call('__src_new')
                m.boot()
                # A stale native snapshot would discard this new buffer.
                self.assertEqual(m.mem[m.symbols['__src_numbuffers']], 2)


if __name__ == '__main__':
    unittest.main()
