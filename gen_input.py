from sys import argv
from math import sqrt

size = int(argv[1])
r_size = int(sqrt(size))

print(f"size {size}\n")

for k in range(r_size):
    for i in range(r_size):
        line = []
        for j in range(size):
            line.append(str(1 + j // r_size + k*r_size))
        print(' '.join(line))
        