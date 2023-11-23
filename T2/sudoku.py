N = 9

def read_sudoku(path):
    matrix = [[0 for i in range(N)] for j in range(N)]
    comparisons = {}
    