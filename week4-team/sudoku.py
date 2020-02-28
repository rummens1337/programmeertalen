import sys
from collections import deque
from copy import deepcopy
from itertools import chain

# Declare global variables.
grid_y_len = 0
grid_x_len = 0
sudoku_max_size = 0
sudoku_min_size = 0
sudoku_set = []


def create_sudoku(filename):
    sudoku = []
    with open(filename) as f:
        [sudoku.append([int(x) for x in line.split()]) for line in f]
    return sudoku


# Initialize global variables, needed in multiple functions.
def initialize_board(board):
    global grid_x_len, grid_y_len, sudoku_max_size, sudoku_min_size, sudoku_set

    sudoku = [1, 2, 3, 4, 5, 6, 7, 8, 9]
    hexadoku = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

    sudoku_max_size = len(board)

    [exit("wrong sudoku size") for row in board if sudoku_max_size != len(row)]

    # Sudoku sizes and their respective values.
    # {sudoku_size} : [{block_x}, {block_y}, {sudoku_set}, {sudoku_minimum}]
    blockSize = {
        4: [2, 2, sudoku, 1],
        6: [3, 2, sudoku, 1],
        9: [3, 3, sudoku, 1],
        12: [4, 3, sudoku, 1],
        16: [4, 4, hexadoku, 0],
        25: [5, 5, hexadoku, 0],
        36: [6, 6, hexadoku, 0],
    }

    grid_x_len, grid_y_len, sudoku_set, sudoku_min_size = blockSize[sudoku_max_size]

# Prints the board


def print_board(board):
    for i in board:
        for n in i:
            print(n, end=" ")
        print()


# Finds empty positions in sudoku.
def find_empty(board):
    for i in range(sudoku_max_size):
        for j in range(sudoku_max_size):
            if board[i][j] == 0:
                return (i, j)  # row, col

    return None

# Checks if num on pos is a valid position


def valid(board, num, pos):
    # Check row
    for i in range(sudoku_max_size):
        if board[pos[0]][i] == num and pos[1] != i:
            return False

    # Check column
    for i in range(sudoku_max_size):
        if board[i][pos[1]] == num and pos[0] != i:
            return False

    # Check grid
    grid_x = pos[1] // grid_x_len
    grid_y = pos[0] // grid_y_len

    for i in range(grid_y * grid_y_len, grid_y * grid_y_len + grid_y_len):
        for j in range(grid_x * grid_x_len, grid_x * grid_x_len + grid_x_len):
            if board[i][j] == num and (i, j) != pos:
                return False

    return True


def row_ok(row):
    for i in row:
        for j in i:
            print(i)
            if sudoku_max_size != len(set(j)):
                exit("wrong sudokoo")

def grid_ok(line):
    grid = []
    grid = ([j for i in line if i for j in i])
    if len(grid) != 0:
        print(max(grid)+1 == 10 == len(set([0]+grid)))


def check_invalid(board):
    checks = []

    rows = board
    columns = [list(a) for a in zip(*board)]
    grids = []

    for i in range(0, sudoku_max_size + 1, grid_y_len):
        for j in range(0, sudoku_max_size + 1, grid_x_len):
            grid = list(chain(row[j:j+grid_x_len]
                              for row in board[i:i+grid_x_len]))
            grids.append(grid)
    [square for square in grids if not grid_ok(square)]

    checks.append(rows)
    checks.append(columns)



# Solve the sudoku


def solve(board):
    solved = deque()
    solved.append(board)

    if not find_empty(board):
        check_invalid(board)
        print_board(board)
        return True

    while solved:
        board = solved.pop()
        find = find_empty(board)

        if not find:
            print_board(board)
            return True
        else:
            row, col = find

        for i in range(sudoku_min_size, sudoku_max_size + 1):
            if valid(board, i, (row, col)):
                board[row][col] = i
                solved.append(deepcopy(board))
    return True

# 8 hour attempt to use a lot less memory.
# R.I.P attempt, for you were a beautifull one.

# def solve(board):
#     solved = deque()
#     row, col = find_empty(board)
#     solved.append([board, sudoku_min_size, (row, col)])
#     success = False
#     backtrack = False

#     while 1:
#         # Pick element from stack
#         board = solved.pop()
#         tmp_board = deepcopy(board[0])  # Finding this error has taken us ages..
#         find = find_empty(tmp_board)
#         print(find)

#         if backtrack:
#         if not find:
#             return True
#         else:
#             row, col = find
#             minimum = sudoku_min_size

#         solved.append([deepcopy(tmp_board), minimum, (row, col)])

#         for i in range(minimum, sudoku_max_size + 1):
#             if valid(tmp_board, i, (row, col)):
#                 print(i)
#                 tmp_board[row][col] = i
#                 solved.append([deepcopy(tmp_board), i, (row, col)])
#                 print(solved)
#                 success = True
#                 break

#         if(success):
#             success = False
#             print("succes")
#         else:
#             solved.pop()
#             backtrack = True


# input/output formaat (ontwikkelend)
# filename = sys.argv[1]

# filename = "sudoku_boards/21_open_spots_9_grid.txt"
# filename = "sudoku_boards/simple_6_grid.txt"
# filename = "sudoku_boards/empty_4_grid.txt"
# filename = "sudoku_boards/empty_16_grid.txt"
filename = "sudoku_boards/16_grid.txt"
# filename = "sudoku_boards/complete_9_grid.txt"
# filename = "sudoku_boards/wrong_9_grid.txt"
# filename = "sudoku_boards/hard_9_grid.txt"
board = create_sudoku(filename)
initialize_board(board)
# solve(board)
solvable(board)
