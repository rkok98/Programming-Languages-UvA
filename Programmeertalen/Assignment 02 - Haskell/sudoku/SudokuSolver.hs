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
centerOfBlocks = [1, 4, 7]

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
main =
    do args <- getArgs
       sud <- (readSudoku . getSudokuName) args
       -- TODO: Call your solver.
       printSudoku sud
       print $ constraints sud
       printSudoku (solveSudoku sud)

freeInLs :: [Value] -> [Value]
freeInLs ls = values \\ ls 

subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) = 
  [ s (r',c') | r' <- (\x -> concat $ filter (elem x) blocks ) r, 
                c' <- (\x -> concat $ filter (elem x) blocks ) c ]

freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r = freeInLs [ s (r,i) | i <- positions ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = freeInLs [ s (i,c) | i <- positions ]

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInLs (subGrid s (r,c))

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) = freeInRow s r `intersect` freeInColumn s c `intersect` freeInSubgrid s (r,c)

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions,  
                            c <- positions, 
                            s (r,c) == openPosition ]

rowValid :: Sudoku -> Row -> Bool
rowValid s r = null (freeInRow s r)

colValid :: Sudoku -> Column -> Bool
colValid s c = null (freeInColumn s c)

subgridValid :: Sudoku -> (Row,Column) -> Bool
subgridValid s (r,c) = null (freeInSubgrid s (r,c))

consistent :: Sudoku -> Bool
consistent s = and $ [ rowValid s r | r <- positions ] ++ [ colValid s c | c <- positions] ++ [ subgridValid s (r,c) | r <- positions, c <- positions ]

-- Solver part

-- Prints a node
printNode :: Node -> IO() 
printNode = printSudoku . fst

-- Calculates all constraints for a given sudoku sorted by solutions length
constraints :: Sudoku -> [Constraint]
constraints s = sortBy solutionsLengthComparable [(r, c, freeAtPos s (r,c)) | (r,c) <- openPositions s ]

-- Comparable that compares the length of the solutions array in a constraint
solutionsLengthComparable :: Constraint -> Constraint -> Ordering
solutionsLengthComparable (_, _, sols) (_, _, sols') = compare (length sols) (length sols')


-- makeTree :: [Constraint] -> Tree

-- 1. Make binary tree from list
-- 2. Traverse through binary tree
 
solveSudoku :: Sudoku -> Sudoku
solveSudoku sud = solve (sud, constraints sud)

solve :: Node -> Sudoku
solve (s, cs) = extendNode (s, tail cs) (r, c, head v)
  where (r, c, v) = head cs

-- data Node = (Sudoku, [Constraint])
-- extend :: Sudoku -> (Row, Column, Value) -> Sudoku
extendNode :: Node -> (Row, Column, Value) -> Sudoku
extendNode (s, cs) (r, c, v) = extend s (r, c, v)