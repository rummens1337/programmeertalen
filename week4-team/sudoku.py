import sys

# Declare global variables.
box_y_len = 0
box_x_len = 0
sudoku_size = 0
sudoku_set = []


def create_sudoku(filename):
    sudoku = []
    with open(filename) as f:
        [sudoku.append([int(x) for x in line.split()]) for line in f]
    return sudoku


# Initialize global variables, needed in multiple functions.
def initialize_board(board):
    global box_x_len, box_y_len, sudoku_size, sudoku_set
    sudoku = [1, 2, 3, 4, 5, 6, 7, 8, 9]
    hexadoku = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 'a', 'b', 'c', 'd', 'e', 'f']

    sudoku_size = len(board)

    [exit("wrong sudoku size") for row in board if sudoku_size != len(row)]

    blockSize = {
        4: [2, 2, sudoku],
        6: [3, 2, sudoku],
        9: [3, 3, sudoku],
        12: [4, 3, sudoku],
        16: [4, 4, hexadoku],
        25: [5, 5, hexadoku],
        36: [6, 6, hexadoku],
    }

    box_x_len, box_y_len, sudoku_set = blockSize[sudoku_size]


def print_board(board):
    for i in board:
        for n in i:
            print(n, end=" ")
        print()


def print_board_pretty(board):
    for i in range(len(board)):
        if i % box_x_len == 0 and i != 0:
            print("- " * int((1.5*sudoku_size)))
        for j in range(len(board[0])):
            if j % box_y_len == 0 and j != 0:
                print(" | ", end="")
            if j == sudoku_size-1:
                print(board[i][j])
            else:
                print(str(board[i][j]) + " ", end="")


# Finds empty positions in sudoku.
def find_empty(board):
    return([[i, j] for i in range(sudoku_size)
            for j in range(sudoku_size) if board[i][j] == 0])


def valid(board, num, pos):
    # Check row
    for i in range(sudoku_size):
        if board[pos[0]][i] == num and pos[1] != i:
            return False

    # Check column
    for i in range(sudoku_size):
        if board[i][pos[1]] == num and pos[0] != i:
            return False

    # Check box
    box_x = pos[1] // box_x_len
    box_y = pos[0] // box_y_len

    for i in range(box_y * box_y_len, box_y * box_y_len + box_y_len):
        for j in range(box_x * box_x_len, box_x * box_x_len + box_x_len):
            print(board[i][j], num, pos)
            if board[i][j] == num and (i, j) != pos:
                return False

    return True


def solve(board):
    num = 1
    while find_empty(board):
        num += 1
        print("still going", num)
        for row, col in find_empty(board):
            for i in range(1, sudoku_size + 1):
                if valid(board, i, (row, col)):
                    board[row][col] = i
                    if not find_empty(board):  # success
                        print("\nOUTPUT:")
                        print_board_pretty(board)
                        return


# input/output formaat (ontwikkelend)
filename = sys.argv[1]
board = create_sudoku(filename)
initialize_board(board)
print_board(board)

# Het algoritme (competend)
solve(board)
