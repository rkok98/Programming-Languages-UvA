import math

open_position = 0

# Reads a sudoku from file
# returns the sudoku as matrix and size of the sudoku


def sudokuToArray(filename):
    sudoku = []

    with open(filename) as f:
        lines = f.readlines()

        for row in lines:
            row = row.rstrip()
            row = list(map(int, row.split(' ')))
            sudoku.append(row)

    size = len(sudoku)

    return sudoku, size

# Prints sudoku


def print_sudoku(sudoku):
    for _, row in enumerate(sudoku):
        print(' '.join(map(str, row)))

# Returns a list of sub grid coordinates
def sub_grids(sudoku):
    sqrt = int(math.sqrt(len(sudoku)))
    size = [i for i in range(0, len(sudoku[0]))]

    return [size[i:i + sqrt] for i in range(0, len(size), sqrt)]

def get_grid(sudoku, row, col):
    grids = sub_grids(sudoku)

    grid_row = [i for rows in grids for i in rows if row in rows]
    grid_col = [i for cols in grids for i in cols if col in cols]

    return [sudoku[row][col] for row in grid_row for col in grid_col]


# Free values in given sequence
def free_values(values):
    n = [i for i in range(1, len(values))]

    values_set = set(values)
    n_set = set(n)

    open_values = list(sorted(values_set - n_set))
    open_values.pop(open_position)

    return open_values

# Free values in row
def free_in_row(sudoku, row):
    return free_values(sudoku[row])

# Free values in column
def free_in_col(sudoku, col):
    return free_values([row[col] for row in sudoku])

def free_in_sub_grid(sudoku, row, col):
    return free_values(get_grid(sudoku, row, col))


def main():
    sudoku, size = sudokuToArray(
        '/Users/renekok/Developer/pre-master/Programmeertalen/Assignment 04 - Python/sudoku/sudoku_boards/1_open_spots_9_grid.txt')
    print_sudoku(sudoku)
    print(free_in_row(sudoku, 1))
    print(free_in_col(sudoku, 1))
    print(free_in_sub_grid(sudoku, 1, 1))


if __name__ == "__main__":
    main()
