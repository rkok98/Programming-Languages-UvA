import argparse
import math

open_position = 0


def sudokuToArray(filename):
    sudoku = []

    with open(filename) as f:
        lines = f.readlines()

        for row in lines:
            row = row.rstrip()
            row = list(map(int, row.split(' ')))
            sudoku.append(row)

    return sudoku


def print_sudoku(sudoku):
    for _, row in enumerate(sudoku):
        print(' '.join(map(str, row)))

def extend(sudoku, row, col, value):
    sudoku[row][col] = value
    return sudoku


def sub_grids(sudoku):
    sqrt = int(math.sqrt(len(sudoku)))
    size = [i for i in range(0, len(sudoku[0]))]

    return [size[i:i + sqrt] for i in range(0, len(size), sqrt)]


def sub_grids_start(sudoku):
    sqrt = int(math.sqrt(len(sudoku)))

    return [i for i in range(0, len(sudoku[0]), sqrt)]


def get_grid(sudoku, row, col):
    grids = sub_grids(sudoku)

    grid_row = [i for rows in grids for i in rows if row in rows]
    grid_col = [i for cols in grids for i in cols if col in cols]

    return [sudoku[row][col] for row in grid_row for col in grid_col]


def free_values(values):
    n = [i for i in range(1, len(values) + 1)]

    values_set = set(values)
    n_set = set(n)

    return list(sorted(n_set - values_set))


def free_in_row(sudoku, row):
    return free_values(sudoku[row])


def free_in_col(sudoku, col):
    return free_values([row[col] for row in sudoku])


def free_in_sub_grid(sudoku, row, col):
    return free_values(get_grid(sudoku, row, col))


def free_at_pos(sudoku, row, col):
    _row = free_in_row(sudoku, row)
    _col = free_in_col(sudoku, col)
    _grid = free_in_sub_grid(sudoku, row, col)

    return list(set(_row) & set(_col) & set(_grid))


def open_positions(sudoku):
    return [(row, col) for row, cols in enumerate(sudoku) for col, val in enumerate(cols) if val == open_position]   


def valid_row(sudoku, row):
    return not free_in_row(sudoku, row)


def valid_col(sudoku, col):
    return not free_in_col(sudoku, col)


def valid_sub_grid(sudoku, row, col):
    return not free_in_sub_grid(sudoku, row, col)


def consistent(sudoku):
    for i in range(0, len(sudoku)):
        if (not valid_row(sudoku, i) or not valid_col(sudoku, i)):
            return False

    for i in sub_grids_start(sudoku):
        for j in sub_grids_start(sudoku):
            if (not valid_sub_grid(sudoku, i, j)):
                return False

    return True


def constraints(sudoku):
    constraints = [(pos[0], pos[1], free_at_pos(sudoku, pos[0], pos[1])) for pos in open_positions(sudoku) if free_at_pos(sudoku, pos[0], pos[1])]
    constraints.sort(key=lambda c: len(c[2]))

    return constraints

def solve(sudoku):
    stack = [sudoku]

    while stack:
        top = stack.pop()
        print('----------')
        print_sudoku(top)

        if consistent(top):
            return top

        pos = open_positions(top)[0]

        for val in free_at_pos(top, pos[0], pos[1]):
            top[pos[0]][pos[1]] = val
            stack.append(top)

    return "failure"

                
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('sudoku_string', action="store",
                        help='sudoku string to be parsed.')

    args = parser.parse_args()
    sudoku = args.sudoku_string

    sudoku = sudokuToArray(sudoku)

    sudoku = solve(sudoku)
    print_sudoku(sudoku)
    print(consistent(sudoku))

if __name__ == "__main__":
    main()
