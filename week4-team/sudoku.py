import sys


def create_sudoku(filename):
    with open(filename) as f:
        sudoku = []
        for line in f:
            sudoku.append([int(x) for x in line.split()])
    return sudoku


# Validates if the sudoku has valid dimensions and returns the
# block size for the sudoku.
def get_block_size(board):

    x = len(board)
    [exit("wrong sudoku size") for y in board if x != len(y)]

    blockSize = {
        4: [2, 2],
        6: [3, 2],
        9: [3, 3],
        12: [4, 3],
        16: [4, 4],
        25: [5, 5],
        36: [6, 6],
    }

    block_x, block_y = blockSize[x]

    return(block_x, block_y, x)


def print_board(board):
    _, _, sudokuSize = get_block_size(board)

    for i in range(len(board)):
        for j in range(len(board[0])):
            if j == sudokuSize-1:
                print(board[i][j])
            else:
                print(str(board[i][j]) + " ", end="")


def print_board_pretty(board):
    block_x, block_y, sudokuSize = get_block_size(board)

    for i in range(len(board)):
        if i % block_x == 0 and i != 0:
            print("- " * int((1.5*sudokuSize)))
        for j in range(len(board[0])):
            if j % block_y == 0 and j != 0:
                print(" | ", end="")
            if j == sudokuSize-1:
                print(board[i][j])
            else:
                print(str(board[i][j]) + " ", end="")


def find_empty(board):
    for i in range(len(board)):
        for j in range(len(board[0])):
            if board[i][j] == 0:
                return (i, j)  # row, col


def valid(board, num, pos):
    # Check row
    for i in range(len(board[0])):
        if board[pos[0]][i] == num and pos[1] != i:
            return False

    # Check column
    for i in range(len(board)):
        if board[i][pos[1]] == num and pos[0] != i:
            return False

    # Check box
    box_x = pos[1] // 3
    box_y = pos[0] // 3

    for i in range(box_y*3, box_y*3 + 3):
        for j in range(box_x * 3, box_x*3 + 3):
            if board[i][j] == num and (i, j) != pos:
                return False

    return True


def solve(board):
    while find_empty(board):
        row, col = find_empty(board)

        for i in range(1, 10):
            if valid(board, i, (row, col)):
                board[row][col] = i

                if not find_empty(board):
                    print("\nOUTPUT:")
                    print_board_pretty(board)
                    return

# input/output formaat (ontwikkelend)
filename = sys.argv[1]
board = create_sudoku(filename)
print_board(board)

# Het algoritme (competend)
solve(board)