
# Reads a sudoku from file
# returns the sudoku as matrix and size of the sudoku
def sudokuToArray(filename):
    sudoku = []

    with open(filename) as f:
        lines = f.readlines()

        for row in lines:
            row = row.rstrip()
            sudoku.append(row.split(' '))

    size = len(sudoku)

    return sudoku, size


def freeValues(values, n):
    values_set = set(values)
    n_set = set(n)

    return (values_set - n_set).union(n_set - values_set)

def main():
    print("hello world!")

if __name__ == "__main__":
    sudokuToArray('/Users/renekok/Developer/pre-master/Programmeertalen/Assignment 04 - Python/sudoku/sudoku_boards/1_open_spots_9_grid.txt')
