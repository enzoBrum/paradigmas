lines = []

while True:
    try:
        lines.append([c for c in input()])
    except:
        break

lines = ['[' + ','.join(line) + ']' for line in lines]
print(',\n'.join(lines))