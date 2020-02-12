module SudokuSolver where

import Sudoku
import Data.List
import Data.List.Split
import Data.Maybe

positions, values :: [Int]
positions = [1..9]
values = [1..9]

blocks :: [[Int]]
blocks = [[1..3], [4..6], [7..9]]

showDgt :: Value -> String
showDgt 0 = " "
showDgt d = show d

showSubgridRow:: [Value] -> String
showSubgridRow = unwords . map showDgt

showRow :: [Value] -> String
showRow sr = "| " ++ intercalate " | " (map showSubgridRow $ chunksOf 3 sr) ++ " |"

showGrid :: Grid -> String
showGrid grid = "+-------+-------+-------+\n" ++ intercalate "\n+-------+-------+-------+\n" rows ++ "\n+-------+-------+-------+" where rows = map (intercalate "\n") $ chunksOf 3 $ map showRow grid

sud2grid :: Sudoku -> Grid
sud2grid s = [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ]

grid2sud :: Grid -> Sudoku
grid2sud gr = \(r,c) -> pos gr (r,c)
  where pos :: [[a]] -> (Row,Column) -> a
        pos gr (r,c) = (gr !! (r - 1)) !! (c - 1)

printSudoku :: Sudoku -> IO()
printSudoku = putStrLn . showGrid . sud2grid
