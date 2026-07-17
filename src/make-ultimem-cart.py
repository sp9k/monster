import sys

BLOCK_SIZE = 1024
IMG_SIZE   = 8192*BLOCK_SIZE

layout = {}

if len(sys.argv) != 3:
    print(f'usage: {sys.argv[0]} <infile> <imgfile>')
    exit(1)

# pad the binary to IMG_SIZE bytes
with open(sys.argv[1], 'rb') as file:
	bin = file.read()

with open(sys.argv[2], 'wb') as file:
	file.write(bin)
	file.write(bytearray([0]*(IMG_SIZE-len(bin))))
