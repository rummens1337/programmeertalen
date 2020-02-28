import sys
from collections import deque
from copy import deepcopy

# Declare global variables.
box_y_len = 0
box_x_len = 0
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
    global box_x_len, box_y_len, sudoku_max_size, sudoku_min_size, sudoku_set
    sudoku = [1, 2, 3, 4, 5, 6, 7, 8, 9]
    hexadoku = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

    sudoku_max_size = len(board)

    [exit("wrong sudoku size") for row in board if sudoku_max_size != len(row)]

    # sudoku_size : [block_x, block_y, sudoku_minimum]
    blockSize = {
        4: [2, 2, sudoku, 1],
        6: [3, 2, sudoku, 1],
        9: [3, 3, sudoku, 1],
        12: [4, 3, sudoku, 1],
        16: [4, 4, hexadoku, 0],
        25: [5, 5, hexadoku, 0],
        36: [6, 6, hexadoku, 0],
    }

    box_x_len, box_y_len, sudoku_set, sudoku_min_size = blockSize[sudoku_max_size]


def print_board(board):
    for i in board:
        for n in i:
            print(n, end=" ")
        print()


# Finds empty positions in sudoku.
def find_empty(bo):
    for i in range(sudoku_max_size):
        for j in range(sudoku_max_size):
            if bo[i][j] == 0:
                return (i, j)  # row, col

    return None


def valid(board, num, pos):
    # Check row
    for i in range(sudoku_max_size):
        if board[pos[0]][i] == num and pos[1] != i:
            return False

    # Check column
    for i in range(sudoku_max_size):
        if board[i][pos[1]] == num and pos[0] != i:
            return False

    # Check box
    box_x = pos[1] // box_x_len
    box_y = pos[0] // box_y_len

    for i in range(box_y * box_y_len, box_y * box_y_len + box_y_len):
        for j in range(box_x * box_x_len, box_x * box_x_len + box_x_len):
            if board[i][j] == num and (i, j) != pos:
                return False

    return True


def solve(board):
    solved = deque()
    solved.append(board)

    while solved:
        # Pick element from stack
        board = solved.pop()
        # board = deepcopy(bo)

        find = find_empty(board)

        if not find:
            print("----------------")
            print_board(board)
            return True
        else:
            row, col = find

        for i in range(sudoku_min_size, sudoku_max_size + 1):
            if valid(board, i, (row, col)):
                # print([row,col], i)
                board[row][col] = i
                solved.append(deepcopy(board))
    return True
    

# def solve(board):
#     solved = deque()
#     row, col = find_empty(board)
#     solved.append([board, sudoku_min_size, (row, col)])
#     success = False

#     while 1:
#         # Pick element from stack
#         bo = solved.pop()
#         tmp_board = deepcopy(bo[0])  # Finding this error has taken us ages..
#         find = find_empty(tmp_board)
#         print(find)

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

# def solve(bo):
#     find = find_empty(bo)
#     if not find:
#         return True
#     else:
#         row, col = find

#     for i in range(1,10):
#         if valid(bo, i, (row, col)):
#             bo[row][col] = i

#             if solve(bo):
#                 return True

#             bo[row][col] = 0

#     return False


def test():
    minimum = 6
    for i in range(minimum, 10):
        print(i)


# input/output formaat (ontwikkelend)
# filename = sys.argv[1]
# filename = "sudoku_boards/21_open_spots_9_grid.txt"
# filename = "sudoku_boards/simple_6_grid.txt"
# filename = "sudoku_boards/empty_4_grid.txt"
# filename = "sudoku_boards/empty_16_grid.txt"
# filename = "sudoku_boards/16_grid.txt"
filename = "sudoku_boards/hard_9_grid.txt"
board = create_sudoku(filename)
initialize_board(board)
print_board(board)

# Het algoritme (competend)
if(solve(board)):
    print("solution")
print_board(board)

# test()
