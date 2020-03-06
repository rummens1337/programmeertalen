"""
Namen: Thomas Vos, Michel Rummens
Studentnummers: 12829501, 13108093

Sudoku solver for the sizes {4, 6, 9, 12, 16, 25, 36}.
Although, you might have to wait a little while for the 36x36 ones
Grab a coffee in the meanwhile.. :)
"""

import sys
from collections import deque
from itertools import chain
import pickle

# Declare global variables.
grid_y_len = 0
grid_x_len = 0
sudoku_max_size = 0
sudoku_min_size = 0
sudoku_set = []


def create_sudoku(filename):
    """
    Creates a list from an input file of integers.
    Exits the application if wrong format is given as input.
    A correct inputfile consists of integer values separated by spaces,
    as instruced per assignment.

    Parameters:
    filename (string): Filename that holds the sudoku.

    Returns:
    list: A list containing integer values, representing the sudoku.
    """

    sudoku = []
    with open(filename) as f:
        try:
            [sudoku.append([int(x) for x in line.split()]) for line in f]
        except ValueError:
            exit("Wrong input.. expecting integer values.")
    if not sudoku:
        exit("Empty file.. baby Yoda dissaproves this input.")
    return sudoku


def initialize_board(board):
    """
    Initializes global variables needed in multiple functions.

    Parameters:
    board (list): The sudoku board to be solved.

    Returns:
    none: Default return value of a python function that has no return.
    """

    global grid_x_len, grid_y_len, sudoku_max_size, sudoku_min_size, sudoku_set

    sudoku = [1, 2, 3, 4, 5, 6, 7, 8, 9]
    hexadoku = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

    sudoku_max_size = len(board)

    [exit("wrong sudoku size..")
     for row in board if sudoku_max_size != len(row)]

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

    grid_x_len, \
        grid_y_len, sudoku_set, sudoku_min_size = blockSize[sudoku_max_size]


def print_board(board):
    """
    Prints the values in the board to the standard output device.

    Parameters:
    board (list): The sudoku board to be solved.

    Returns:
    none: Default return value of a python function that has no return.
    """

    for i in board:
        for n in i:
            print(n, end=" ")
        print()


def find_empty(board):
    """
    Finds the first empty spot in the sudoku puzzle.
    It searches from top-left to bottom-right.

    Parameters:
    board (list): The sudoku board to be solved.

    Returns:
    none: Default return value of a python function that has no return.
    """

    for i in range(sudoku_max_size):
        for j in range(sudoku_max_size):
            if board[i][j] == 0:
                return (i, j)  # row, col


def valid(board, num, pos):
    """
    Checks whether a given num on a given position (row,col) is valid.
    A valid position means there are no duplicates within the same
    row, column or subgrid.

    Parameters:
    board (list): The sudoku board to be solved.
    num (int): The number to be inserted into the sudoku board.
    pos (touple): The position to be inserted into (row,col)

    Returns:
    bool: True if num is a valid position
          False if num is not a valid position
    """

    # # Check row
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


def grid_valid(line):
    """
    Checks whether a subgrid is valid.
    If it's not valid, exits the application.

    Parameters:
    line (list): The subgrid to be checked.

    Returns:
    none: Default return value of a python function that has no return.
    """

    grid = []
    grid = ([j for i in line if i for j in i])
    if len(grid) != 0:
        if not (max(grid)+1 == 10 == len(set([0]+grid))):
            exit("Faulty value(s) in grid..")


def check_valid(board):
    """
    Checks whether the completed sudoku is a valid solution.

    Parameters:
    board (list): A fully completed sudoku board.

    Returns:
    none: Default return value of a python function that has no return.
    """

    checks = []
    checks.append(board)
    checks.append([list(a) for a in zip(*board)])

    # Check whether any row/column contains duplicates, thus resulting
    # in a non-valid sudoku.
    [exit("Faulty value(s) in row or column..")
     for i in checks for j in i if sudoku_max_size != len(set(j))]

    # Check whether any sub grid contains duplicates, thus resulting
    # in a non-valid sudoku.
    for i in range(0, sudoku_max_size + 1, grid_y_len):
        for j in range(0, sudoku_max_size + 1, grid_x_len):
            grid = list(chain(row[j:j+grid_x_len]
                              for row in board[i:i+grid_x_len]))
            grid_valid(grid)


def solve(board):
    """
    Tries to solve the sudoku by inserting values into free positions.
    If no valid value can be found, backtrack into the stack's last value
    by popping the stack.
    By using a stack, the working of a recursive function is mimicked.

    Parameters:
    board (list): The sudoku board to be solved.

    Returns:
    bool: True if a valid solution is found.
    """

    stack = deque()
    stack.append(board)

    # If input board is already completed, check if it's a valid solution.
    if not find_empty(board):
        check_valid(board)
        print_board(board)
        return True

    while stack:
        board = stack.pop()
        find = find_empty(board)

        # Print board if solution is found, otherwise continue traversal.
        if not find:
            print_board(board)
            return True
        else:
            row, col = find

        # Check if i is a valid position, if so apply it to the stack.
        # Otherwise it will be popped when continueing in the loop,
        # Thus removing it as a possibility from the search tree.
        for i in range(sudoku_min_size, sudoku_max_size + 1):
            if valid(board, i, (row, col)):
                board[row][col] = i
                stack.append(pickle.loads(pickle.dumps(board)))
    return True


# 8 hour attempt to use a lot less memory and speed up process.
# R.I.P attempt, for you were a beautifull one.

# def solve(board):
#     stack = deque()
#     row, col = find_empty(board)
#     stack.append([board, sudoku_min_size, (row, col)])
#     success = False
#     backtrack = False

#     while 1:
#         # Pick element from stack
#         board = stack.pop()
#         tmp_board = deepcopy(board[0])  # Finding this error has
# taken us ages..
#         find = find_empty(tmp_board)
#         print(find)

#         if backtrack:
#         if not find:
#             return True
#         else:
#             row, col = find
#             minimum = sudoku_min_size

#         stack.append([deepcopy(tmp_board), minimum, (row, col)])

#         for i in range(minimum, sudoku_max_size + 1):
#             if valid(tmp_board, i, (row, col)):
#                 print(i)
#                 tmp_board[row][col] = i
#                 stack.append([deepcopy(tmp_board), i, (row, col)])
#                 print(stack)
#                 success = True
#                 break

#         if(success):
#             success = False
#             print("succes")
#         else:
#             stack.pop()
#             backtrack = True


filename = sys.argv[1]
# filename = "sudoku_boards/21_open_spots_9_grid.txt"
# filename = "sudoku_boards/simple_6_grid.txt"
# filename = "sudoku_boards/empty_4_grid.txt"
# filename = "sudoku_boards/empty_16_grid.txt"
# filename = "sudoku_boards/16_grid.txt"
# filename = "sudoku_boards/sudoku_string.txt"
# filename = "sudoku_boards/empty_file.txt"
# filename = "sudoku_boards/complete_9_grid.txt"
# filename = "sudoku_boards/wrong_9_grid.txt"
# filename = "sudoku_boards/hard_9_grid.txt"
# filename = "sudoku_boards/36_grid.txt"

board = create_sudoku(filename)
initialize_board(board)
solve(board)
