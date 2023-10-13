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
        

matrix = [[0 for _ in range(9)] for __ in range(9)]

for i in range(9):
    line = input().replace('[', '').replace(']', '')
    line = [int(x) for x in line.split(',')]
    for j in range(9):
        matrix[i][j] = line[j]

for i in range(9):
    line = []
    for j in range(8):
        if (j+1) % 3 != 0:
            s = '<' if matrix[i][j] < matrix[i][j+1] else '>'
            line.append(s)
    if line:
        print(' '.join(line))

print()
for i in range(8):
    line = []
    for j in range(9):
        if ( i + 1 ) % 3 != 0:
            s = '^' if matrix[i][j] < matrix[i+1][j] else 'v'
            line.append(s)
    if line:
        print(' '.join(line))


