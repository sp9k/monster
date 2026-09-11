"""Extract an FE3 disk build using the loader's own bank-copy table."""
import argparse
from pathlib import Path
import subprocess


def read_labels(path):
    labels = {}
    for line in Path(path).read_text().splitlines():
        _, address, name = line.split()
        labels.setdefault(name.lstrip('.'), int(address, 16))
    return labels


def extract(image, labels):
    if len(image) != 512 * 1024:
        raise ValueError('FE3 linker output must contain sixteen 32 KiB banks')

    def bank_bytes(bank, address, size):
        if not (0x2000 <= address < address + size <= 0x8000 or
                0xa000 <= address < address + size <= 0xc000):
            raise ValueError('Payload crosses an FE3 memory window')
        offset = (bank & 15) * 32768 + address - (0x4000 if address >= 0xa000 else 0x2000)
        return image[offset:offset + size]

    boot = bank_bytes(0, labels['__FE3BOOT_LOAD__'], labels['__FE3BOOT_SIZE__'])
    start = labels['__FE3BOOT_RUN__']
    if start != 0x1201 or start + len(boot) > 0x2000:
        raise ValueError('Disk loader must fit in internal RAM at $1201')
    files = {'monster': start.to_bytes(2, 'little') + boot}
    first = labels['__fe3_copies'] - start
    last = labels['__fe3_copies_end'] - start
    if not 4 <= first < last <= len(boot) or (last - first) % 4:
        raise ValueError('Invalid loader copy table')
    # The first record stages shared code/data; the remaining records match
    # cartridge startup exactly. Repeated source banks share one disk file.
    for offset in range(first - 4, last, 4):
        source, destination, page, pages = boot[offset:offset + 4]
        if source & 0xf0 != 0x40 or destination & 0xf0 != 0xa0:
            raise ValueError('Invalid FE3 copy bank')
        address, size = page << 8, pages << 8
        name = f'm{source & 15:x}{page:02x}'
        payload = address.to_bytes(2, 'little') + bank_bytes(source, address, size)
        if name in files and files[name] != payload:
            raise ValueError(f'Conflicting payloads for {name}')
        files[name] = payload
    return files


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('image', type=Path)
    parser.add_argument('labels', type=Path)
    parser.add_argument('directory', type=Path)
    parser.add_argument('disk', type=Path)
    parser.add_argument('--c1541', default='c1541')
    args = parser.parse_args()
    files = extract(args.image.read_bytes(), read_labels(args.labels))
    args.directory.mkdir(parents=True, exist_ok=True)
    command = [args.c1541, '-format', 'monster fe3,01', 'd64', str(args.disk),
               '-attach', str(args.disk)]
    for name, data in files.items():
        path = args.directory / (name + '.prg')
        path.write_bytes(data)
        command.extend(('-write', str(path), name))
    subprocess.run(command, check=True)
    print(f'Created {args.disk}: {len(files)} files, {sum(map(len, files.values())):,} bytes')


if __name__ == '__main__':
    main()
