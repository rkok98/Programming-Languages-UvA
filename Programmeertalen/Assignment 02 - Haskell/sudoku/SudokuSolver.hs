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

subGrid :: Sudoku -> (Row, Column) -> [Value]
subGrid sud (row, col) = [ sud (row', col') | row' <- concat $ filter(elem row) blocks, col' <- concat $ filter(elem col) blocks ]

freeInRow :: Sudoku -> Row -> [Value]
freeInRow sud row = values \\ [ sud (row, pos) | pos <- positions ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn sud col = values \\ [ sud (pos, col) | pos <- positions ]

freeInSubgrid :: Sudoku -> (Row, Column) -> [Value]
freeInSubgrid sud (row, col) = values \\ subGrid sud (row, col)

freeAtPos :: Sudoku -> (Row, Column) -> [Value]
freeAtPos sud (row, col) = freeInRow sud row `intersect` 
                           freeInColumn sud col `intersect`
                           freeInSubgrid sud (row, col)

openPositions :: Sudoku -> [(Row, Column)]
openPositions sud = [ (row, col) | row <- positions,  
                                   col <- positions, 
                                   sud (row, col) == openPosition ]

rowValid :: Sudoku -> Row -> Bool
rowValid sud row = null (freeInRow sud row)

colValid :: Sudoku -> Column -> Bool
colValid sud col = null (freeInColumn sud col)

subgridValid :: Sudoku -> (Row, Column) -> Bool
subgridValid sud (row, col) = null (freeInSubgrid sud (row, col))

consistent :: Sudoku -> Bool
consistent sud = and $ [ rowValid sud row | row <- positions ] ++ 
                       [ colValid sud col | col <- positions] ++ 
                       [ subgridValid sud (row, col) | row <- positions, col <- positions ]

-- Prints a node
printNode :: Node -> IO() 
printNode = printSudoku . fst

-- Calculates all constraints for a given sudoku sorted by solutions length
constraints :: Sudoku -> [Constraint]
constraints sud = sortBy solsLengthComparable [(row, col, freeAtPos sud (row, col)) | (row, col) <- openPositions sud ]

-- Comparable that compares the length of the solutions array in a constraint
solsLengthComparable :: Constraint -> Constraint -> Ordering
solsLengthComparable (_, _, sols) (_, _, sols') = compare (length sols) (length sols')

solveSudoku :: Sudoku -> Solver -> Sudoku
solveSudoku sud solver | consistent solution = solution
                       | otherwise = error "Unsolvable sudoku!"
                where solution = head (solve sud solver)

solve :: Sudoku -> Solver -> [Sudoku]
solve sud solver | null (openPositions sud) = [sud]
          | otherwise = concatMap  (`solve` solver) (subtree sud solver) --solve $ subtree sud solver

-- Generate 
subtree :: Sudoku -> Solver -> [Sudoku]
subtree sud solver = [extend sud (row, col, v) | v <- solver sud (row, col)]
    where (row, col) = head (openPositions sud)

-- Solve NRC
type Solver = Sudoku -> (Row, Column) -> [Value]

nrcSolver :: Solver
nrcSolver = freeAtPosNrc

normalSolver :: Solver
normalSolver = freeAtPos

getSolver :: [String] -> Solver
getSolver xs
    | null xs = normalSolver
    | head xs == "nrc" = nrcSolver
    | head xs == "normal" = normalSolver

nrcBlocks :: [[Int]]
nrcBlocks = [[2..4], [6..8]]

nrcGrid :: Sudoku -> (Row, Column) -> [Value]
nrcGrid sud (row, col) = [ sud (row', col') | row' <- concat $ filter(elem row) nrcBlocks, col' <- concat $ filter(elem col) nrcBlocks ]

freeInNrcgrid :: Sudoku -> (Row, Column) -> [Value]
freeInNrcgrid sud (row, col) = values \\ nrcGrid sud (row, col)

freeAtPosNrc :: Sudoku -> (Row, Column) -> [Value]
freeAtPosNrc sud (row, col) = freeInNrcgrid sud (row, col) `intersect` freeAtPos sud (row, col) 