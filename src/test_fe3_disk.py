"""Run FE3 regressions from disk payloads with KERNAL file I/O emulated.

The FE3 flash is erased throughout; all application bytes must come from files.
Run: make test-fe3-disk PYTHON=myenv/bin/python
"""
from pathlib import Path
import unittest
from unittest.mock import patch
import test_fe3 as fe3


class DiskMachine(fe3.Machine):
    def __init__(self, device=8, failure=None):
        super().__init__(image=bytes([255]) * (512 * 1024),
                         labels=Path('labels-fe3-disk.txt').read_text().splitlines())
        self.mem[0xba] = device
        self.failure = failure
        self.requests = []
        self.messages = bytearray()

    def boot(self):
        boot = Path('fe3-disk/monster.prg').read_bytes()
        start = int.from_bytes(boot[:2], 'little')
        for i, value in enumerate(boot[2:]):
            self.mem[start+i] = value
        self.cpu.pc = self.symbols['__fe3_init']
        self.mem.config = self.mem.reg = 0
        self.mem.locked = True
        # A disk launch follows KERNAL/BASIC startup, including after reset.
        self.mem[0x98], self.mem[0x99], self.mem[0x9a] = 0, 0, 3
        filename = ''
        secondary = None

        def setnam():
            nonlocal filename
            start = self.cpu.x | self.cpu.y << 8
            filename = bytes(self.mem[start:start+self.cpu.a]).decode().lower()

        def setlfs():
            nonlocal secondary
            secondary = self.cpu.y
            self.mem[0xba] = self.cpu.x

        def load():
            assert secondary == 0
            assert self.cpu.a == 0
            self.requests.append((filename, self.mem[0xba], self.mem.reg))
            if self.failure == 'missing':
                self.failure = None
                self.cpu.a = 4
                self.cpu.p |= 1
                return
            data = Path(f'fe3-disk/{filename}.prg').read_bytes()[2:]
            if self.failure == 'short':
                self.failure = None
                data = data[:-1]
            address = self.cpu.x | self.cpu.y << 8
            for i, value in enumerate(data):
                self.mem[address+i] = value
            end = address + len(data)
            self.cpu.x, self.cpu.y = end & 255, end >> 8
            self.cpu.p &= ~1
            # Exercise the loader's independence from KERNAL's ZP workspace.
            for address in range(0x90, 0xb0):
                self.mem[address] = 0xcc

        def chrout():
            self.messages.append(self.cpu.a)

        def getin():
            self.cpu.a = 13
            self.cpu.p &= ~2

        saved = self.io_hooks
        self.io_hooks = {0xffbd: setnam, 0xffba: setlfs, 0xffd5: load,
                         0xffd2: chrout, 0xffe4: getin, 0xffcc: lambda: None}
        self.run_until(self.symbols['enter'])
        self.io_hooks = saved


class FE3Disk(fe3.FE3):
    def setUp(self):
        self.machine_patch = patch.object(fe3, 'Machine', DiskMachine)
        self.machine_patch.start()
        self.addCleanup(self.machine_patch.stop)

    def test_boot_relocations(self):
        m = DiskMachine()
        m.boot()
        self.assertEqual(m.mem.reg, 0xa1)
        data = Path('fe3-disk/m0a0.prg').read_bytes()[2:]
        for segment in ('BANKCODE', 'BANKCODE2', 'IRQ', 'DATA', 'FE3CFG'):
            load = m.symbols[f'__{segment}_LOAD__'] - 0xa000
            run = m.symbols[f'__{segment}_RUN__']
            size = m.symbols[f'__{segment}_SIZE__']
            self.assertEqual(bytes(m.mem[run:run+size]), data[load:load+size], segment)

    def test_all_banks_loaded(self):
        m = DiskMachine()
        m.boot()
        image = Path('monster-fe3-disk.prg').read_bytes()
        # Independent runtime map, including code duplicates and untouched RAM.
        for source, dest, first, size in (
                (0, 0, 0x4000, 0x2000), (1, 1, 0x2000, 0x5e00),
                (1, 1, 0xa000, 0x2000), (2, 2, 0xa000, 0x2000),
                (3, 3, 0x7000, 0x1000), (3, 3, 0xa000, 0x2000),
                (4, 4, 0xa000, 0x1800), (5, 5, 0xa000, 0x2000),
                (6, 6, 0xa000, 0x2000), (6, 7, 0xa000, 0x2000),
                (8, 8, 0xa000, 0x2000), (9, 9, 0xa000, 0x2000),
                (11, 0, 0xa000, 0x1800),
                *((1, dest, 0xa000, 0x2000) for dest in range(11, 16))):
            offset = first - (0x4000 if first >= 0xa000 else 0x2000)
            expected = image[source*32768+offset:source*32768+offset+size]
            actual = m.mem.ram[dest*32768+offset:dest*32768+offset+size]
            self.assertEqual(actual, expected, (source, dest, hex(first)))
        untouched = b'\xff\x00' * (0x6000 // 2)
        for bank in range(10, 16):
            self.assertEqual(m.mem.ram[bank*32768:bank*32768+0x6000], untouched)

    def test_loading_device(self):
        for device, expected in ((0, 8), (1, 8), (8, 8), (9, 9), (11, 11)):
            m = DiskMachine(device=device)
            m.boot()
            self.assertEqual(m.mem[0xba], expected)
            self.assertEqual({r[1] for r in m.requests}, {expected})

    def test_native_restart_starts_fresh(self):
        m = self.machine()
        m.call('__src_new')
        for i, byte in enumerate(bytes.fromhex('4c 00 12')):
            m.call('__vmem_store', a=byte, xy=0x1200+i)
        m.mem[m.symbols['__sim_pc']] = 0
        m.mem[m.symbols['__sim_pc']+1] = 0x12
        m.cpu.pc = m.symbols['__run_go']
        m.run_until(0x1200)
        m.boot()
        m.cpu.step()
        m.run_until(m.symbols['__edit_run'])
        self.assertEqual(m.mem[m.symbols['__src_numbuffers']], 1)
        m.mem.reg = 0xa9
        self.assertEqual(m.mem[m.symbols['__fe3_native_saved']], 0)

    def test_load_retry(self):
        for failure in ('missing', 'short'):
            m = DiskMachine(failure=failure)
            m.boot()
            self.assertIn(b'LOAD ERROR', m.messages)
            self.assertEqual(m.requests[0], m.requests[1])
            self.assertEqual(len(m.requests), 20)
            self.assertEqual(m.mem.reg, 0xa1)


if __name__ == '__main__':
    unittest.main()
