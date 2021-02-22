
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
def printSudoku(sudoku):
    for _, row in enumerate(sudoku):
        print(' '.join(map(str, row)))

def freeValues(values):
    n = [item for item in range(1, len(values))] 

    values_set = set(values)
    n_set = set(n)

    return (values_set - n_set).union(n_set - values_set)

def freeInRow(sudoku, row):
    return freeValues(sudoku[row])



def main():
    print("hello world!")

if __name__ == "__main__":
    sudoku, size = sudokuToArray('/Users/renekok/Developer/pre-master/Programmeertalen/Assignment 04 - Python/sudoku/sudoku_boards/1_open_spots_9_grid.txt')
    print(sudoku)
    printSudoku(sudoku)
    print(freeInRow(sudoku, 1))


