import sys
import math
import copy

open_position = 0


def sudokuToArray(filename):
    '''
    Generates a sudoku matrix from a given sudoku file.

    Returns:
        sudoku
    '''
    with open(filename) as file:
        sudoku = []
        lines = file.readlines()

        for row in lines:
            row = row.rstrip()
            row = list(map(int, row.split(' ')))
            sudoku.append(row)

    return sudoku


def print_sudoku(sudoku):
    '''
    Print given sudoku.
    '''
    for _, row in enumerate(sudoku):
        print(' '.join(map(str, row)))

    print()


def extend(sudoku, row, col, value):
    '''
    Fills in the given value in a given spot in the sudoku.
    '''
    sudoku[row][col] = value
    return sudoku


def sub_grids(sudoku):
    '''
    Returns the subgrids of a sudoku.
    '''
    sqrt = int(math.sqrt(len(sudoku)))
    size = [i for i in range(0, len(sudoku[0]))]

    return [size[i:i + sqrt] for i in range(0, len(size), sqrt)]


def sub_grids_start(sudoku):
    '''
    Returns the start coordinate of every subgrid.
    '''
    sqrt = int(math.sqrt(len(sudoku)))

    return [i for i in range(0, len(sudoku[0]), sqrt)]


def get_grid(sudoku, row, col):
    '''
    Returns the values inside a sub grid.
    '''
    grids = sub_grids(sudoku)

    grid_row = [i for rows in grids for i in rows if row in rows]
    grid_col = [i for cols in grids for i in cols if col in cols]

    return [sudoku[row][col] for row in grid_row for col in grid_col]


def free_values(sequence):
    '''
    Returns the remaining values of a given sequence.
    '''
    n = [i for i in range(1, len(sequence) + 1)]

    sequence_set = set(sequence)
    n_set = set(n)

    return list(sorted(n_set - sequence_set))


def free_in_row(sudoku, row):
    '''
    Returns the remaining values of a given row.
    '''
    return free_values(sudoku[row])


def free_in_col(sudoku, col):
    '''
    Returns the remaining values of a given column.
    '''
    return free_values([row[col] for row in sudoku])


def free_in_sub_grid(sudoku, row, col):
    '''
    Returns the remaining values of a given sub grid.
    '''
    return free_values(get_grid(sudoku, row, col))


def free_at_pos(sudoku, row, col):
    '''
    Returns the possible values of a given position based on 
    the remaining possible values of a row, column and sub grid.
    '''
    _row = free_in_row(sudoku, row)
    _col = free_in_col(sudoku, col)
    _grid = free_in_sub_grid(sudoku, row, col)

    return list(set(_row) & set(_col) & set(_grid))


def open_positions(sudoku):
    '''
    Returns a list of coordinates of every empty spot in a sudoku.
    '''
    return [(row, col) for row, cols in enumerate(sudoku)
            for col, val in enumerate(cols) if val == open_position]


def valid_row(sudoku, row):
    '''
    Returns if a given row is valid.
    '''
    return not free_in_row(sudoku, row)


def valid_col(sudoku, col):
    '''
    Returns if a given column is valid.
    '''
    return not free_in_col(sudoku, col)


def valid_sub_grid(sudoku, row, col):
    '''
    Returns if a given sub grid is valid.
    '''
    return not free_in_sub_grid(sudoku, row, col)


def consistent(sudoku):
    '''
    Returns if a the given sudoku is consistent.
    '''
    for i in range(0, len(sudoku)):
        if not (valid_row(sudoku, i) or valid_col(sudoku, i)):
            return False

    for i in sub_grids_start(sudoku):
        for j in sub_grids_start(sudoku):
            if (not valid_sub_grid(sudoku, i, j)):
                return False

    return True


def constraints(sudoku):
    '''
    Returns a list of all possible solutions of all empty spots in a sudoku.
    '''
    constraints = [(pos[0], pos[1], free_at_pos(sudoku, pos[0], pos[1]))
                   for pos in open_positions(sudoku)]
    constraints.sort(key=lambda c: len(c[2]))

    return constraints


def solve(sudoku, amount_sols):
    '''
    Solves a sudoku using backtracking and a stack.

    Returns:
        solutions: A list of n amount of solutions; An error if the sudoku is unsolvable
    '''
    stack = []
    solutions = []
    stack.append(sudoku)

    while stack:
        sud = stack.pop()

        if consistent(sud):
            solutions.append(sud)

            if len(solutions) is amount_sols:
                return solutions

        cons = constraints(sud)
        if cons:
            constraint = cons[0]
            for val in constraint[2]:
                row = constraint[0]
                col = constraint[1]

                s = copy.deepcopy(sud)

                s = extend(s, row, col, val)
                stack.append(s)

    if solutions:
        return solutions
    else:
        raise ValueError


def main():
    sudoku, amount_sols = parse_args()

    sudoku = sudokuToArray(sudoku)
    sudokus = solve(sudoku, amount_sols)

    for sudoku in sudokus:
        print_sudoku(sudoku)


def parse_args():
    '''
    Parse given arguments.

    Returns:
        sudoku_file: The path to the given sudoku file
        solutions_amount: Amount of requested solutions,
                          returns 1 if argument is not given
    '''
    sudoku_file = sys.argv[1]
    solutions_amount = sys.argv[2] if len(sys.argv) > 2 else 1

    return sudoku_file, solutions_amount


if __name__ == "__main__":
    main()
