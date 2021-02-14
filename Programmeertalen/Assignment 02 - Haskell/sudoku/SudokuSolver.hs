-- Author:   René Kok (13671146)
-- Study:    Doorstroomminor Software Engineering UvA
-- 
-- This script solves simple sudoku's by using a search tree.

import System.Environment
import Data.List
import Data.Maybe

type Row = Int
type Column = Int
type Value = Int
type Grid = [[Value]]
type Sudoku = (Row,Column) -> Value
type Constraint = (Row, Column, [Value])
type Node = (Sudoku, [Constraint])
type Solver = Sudoku -> (Row, Column) -> [Value]

positions :: [Int]
positions = [1..9]

values :: [Value]
values = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

centerOfBlocks :: [Int]
centerOfBlocks = [2, 5, 8]

openPosition :: Int
openPosition = 0

-- Converts a sudoku into a grid.
sud2grid :: Sudoku -> Grid
sud2grid s = [[s (r, c) | c <- positions] | r <- positions]

-- Converts a grid into a sudoku.
grid2sud :: Grid -> Sudoku
grid2sud gr = \(r, c) -> pos gr (r, c)
  where pos :: [[a]] -> (Row,Column) -> a
        pos gr (r, c) = (gr !! (r - 1)) !! (c - 1)

-- Function to extend a sudoku with a value at (row, column).
extend :: Sudoku -> (Row, Column, Value) -> Sudoku
extend sud (r, c, v) (i, j) = if r == i && c == j then v else sud (i, j)


-- Read a file-sudoku into a Sudoku
readSudoku :: String -> IO Sudoku
readSudoku filename =
    do stringGrid <- readFile filename
       return $ (grid2sud . splitStringIntoGrid) stringGrid
       where splitStringIntoGrid = map (map readint . words) . lines
             readint x = read x :: Int

-- Prints a Sudoku to the terminal by transforming it to a grid first.
printSudoku :: Sudoku -> IO ()
printSudoku = putStr . unlines . map (unwords . map show) . sud2grid
                

-- Helper to parse command-line arguments.
getSudokuName :: [String] -> String
getSudokuName [] = error "Filename of sudoku as first argument."
getSudokuName (x:_) = x

-- Do not modify the way your sudoku is printed!
main :: IO ()
main =
    do args <- getArgs
       sud <- (readSudoku . getSudokuName) args
       printSudoku (solveSudoku sud (getSolver (tail args)))

-- Returns an array representing the filled in values of a block that contains the given coordinates
getGrids :: Sudoku -> (Row, Column) -> Grid -> [Value]
getGrids sud (row, col) blocks' = [ sud (row', col') | row' <- concat $ filter(elem row) blocks', col' <- concat $ filter(elem col) blocks' ]

-- Returns the remaining possible values of a given list of values
freeVals :: [Value] -> [Value]
freeVals vals = values \\ vals 

-- Returns the remaining possible values of a given row
freeInRow :: Sudoku -> Row -> [Value]
freeInRow sud row = freeVals [ sud (row, pos) | pos <- positions ]

-- Returns the remaining possible values of a given column
freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn sud col = freeVals [ sud (pos, col) | pos <- positions ]

-- Returns the remaining possible values of a given subgrid
freeInSubgrid :: Sudoku -> (Row, Column) -> [Value]
freeInSubgrid sud (row, col) = freeVals (getGrids sud (row, col) blocks)

-- Returns the remaining possible values of a position based on row, column and subgrid
freeAtPos :: Sudoku -> (Row, Column) -> [Value]
freeAtPos sud (row, col) = freeInRow sud row `intersect` 
                           freeInColumn sud col `intersect`
                           freeInSubgrid sud (row, col)

-- Returns a list of open positions in a sudoku
openPositions :: Sudoku -> [(Row, Column)]
openPositions sud = [ (row, col) | row <- positions,  
                                   col <- positions, 
                                   sud (row, col) == openPosition ]

-- Validates given row
rowValid :: Sudoku -> Row -> Bool
rowValid sud row = null (freeInRow sud row)

-- Validates given column
colValid :: Sudoku -> Column -> Bool
colValid sud col = null (freeInColumn sud col)

-- Validates given subgrid
subgridValid :: Sudoku -> (Row, Column) -> Bool
subgridValid sud (row, col) = null (freeInSubgrid sud (row, col))

-- Validates the whole sudoku by validating the rows, columns and subgrids
consistent :: Sudoku -> Bool
consistent sud = and $ [ rowValid sud row | row <- positions ] ++ 
                       [ colValid sud col | col <- positions] ++ 
                       [ subgridValid sud (row, col) | row <- positions, col <- positions ]

-- Prints a node
printNode :: Node -> IO() 
printNode = printSudoku . fst

-- Calculates all constraints for a given sudoku sorted by solutions length
-- Note: Unused
constraints :: Sudoku -> [Constraint]
constraints sud = sortBy solsLengthComparable [(row, col, freeAtPos sud (row, col)) | (row, col) <- openPositions sud ]

-- Comparable that compares the length of the solutions array in a constraint
solsLengthComparable :: Constraint -> Constraint -> Ordering
solsLengthComparable (_, _, sols) (_, _, sols') = compare (length sols) (length sols')

-- Solves solvable sudokus, if the sudoku is not solvable an error will be returned
solveSudoku :: Sudoku -> Solver -> Sudoku
solveSudoku sud solver | consistent solution = solution
                       | otherwise = error "Unsolvable sudoku!"
                where solution = head (solve sud solver)

-- Calculates states of a sudoku and will return the state of a sudoku if it's solved
solve :: Sudoku -> Solver -> [Sudoku]
solve sud solver | null (openPositions sud) = [sud]
                 | otherwise = concatMap  (`solve` solver) (subSudokus sud solver)

-- Generate sub sudokus from sudoku
subSudokus :: Sudoku -> Solver -> [Sudoku]
subSudokus sud solver = [extend sud (row, col, v) | v <- solver sud (row, col)]
    where (row, col) = head (openPositions sud)

-- Solve NRC
nrcSolver :: Solver
nrcSolver = freeAtPosNrc

normalSolver :: Solver
normalSolver = freeAtPos

-- Returns the right solver for the given sudoku type (normal or nrc sudoku)
getSolver :: [String] -> Solver
getSolver xs
    | null xs = normalSolver
    | head xs == "nrc" = nrcSolver
    | head xs == "normal" = normalSolver

nrcBlocks :: Grid
nrcBlocks = [[2..4], [6..8]]

-- Returns the remaining possible values of a given nrc grid
freeInNrcGrid :: Sudoku -> (Row, Column) -> [Value]
freeInNrcGrid sud (row, col) = values \\ getGrids sud (row, col) nrcBlocks

-- Returns the remaining possible values of a position based on row, column, subgrid and nrc grid
freeAtPosNrc :: Sudoku -> (Row, Column) -> [Value]
freeAtPosNrc sud (row, col) = freeInNrcGrid sud (row, col) `intersect` freeAtPos sud (row, col) 