-- Namen: Thomas Vos, Michel Rummens
-- Studentnummers: 12829501, 13108093
-- This program can solve a sudoku using several functions and a binary tree.

module SudokuSolver where

import Sudoku
import Data.List
import Data.Maybe

testGrid :: Grid -- Testgrid, weghalen voor inleveren
testGrid =
  [ [1,0,3,4,5,6,7,8,9]
  , [4,5,6,7,8,9,1,2,3]
  , [7,8,9,1,2,3,4,5,6]
  , [2,3,1,6,7,4,8,9,5]
  , [8,7,5,9,1,2,3,6,4]
  , [6,9,4,5,3,8,2,1,7]
  , [3,1,7,2,6,5,9,4,8]
  , [5,4,2,8,9,7,6,3,1]
  , [9,6,8,3,4,1,5,7,2]
  ]

testSudoku :: Sudoku -- Testsudoku, weghalen voor inleveren
testSudoku = grid2sud testGrid

positions, values :: [Int]
positions = [1..9]
values = [1..9]

blocks :: [[Int]] -- ?
blocks = [[1..3], [4..6], [7..9]]

showDgt :: Value -> String
showDgt 0 = " "
showDgt d = show d

showSubgridRow:: [Value] -> String
showSubgridRow = unwords . map showDgt

{-- The chunksOf n xs ++ chunksOf n ys == chunksOf n (xs ++ ys)
property holds.
--}

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = [] -- Models the Data.List.Split implementation
chunksOf n ls = fst splitted : (chunksOf n . snd) splitted where splitted = splitAt n ls

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

-- FUNCTIONS THAT WERE REQUIRED TO BE ADDED

-- Returns the sudoku with one value changed.

extend :: Sudoku -> (Row,Column,Value) -> Sudoku
extend s (r,c,v) = grid2sud (foldr(\z acc -> if z == r then ((init xs) ++ [v] ++ ys) : acc else (grid !! (z-1)) : acc) [] [1..9])
   where (xs,ys) = splitAt (c) ((sud2grid s) !! (r - 1))
         grid = sud2grid s

-- Returns a list of every number available for one specific row.

freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r = [1..9] \\ ((sud2grid s) !! (r - 1))

-- Returns a list of every number available for one specific column.

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = [1..9] \\ (foldr(\x acc -> x !! (c - 1) : acc) [] (sud2grid s))

-- Returns a list of every number available for one specific subgrid.

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = [1..9] \\ (getValues s (topLeftPoint(r,c)))

-- Returns a list of every number available for one specific position.

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) = (((freeInRow s r) ++ (freeInColumn s c) ++ (freeInSubgrid s (r,c))) \\ [1..9]) \\ [1..9]

-- Returns a list of every open position.

openPositions :: Sudoku -> [(Row,Column)]
openPositions s = concat (foldr(\x acc -> zip [x,x..] (openPosColumn s x) : acc) [] [1..9])



-- Returns if a row is valid (no empty spots and no double numbers).

rowValid :: Sudoku -> Row -> Bool
rowValid s r = (freeInRow s r) == []

-- Returns if a column is valid (no empty spots and no double numbers).

colValid :: Sudoku -> Column -> Bool
colValid s c = (freeInColumn s c) == []

-- Returns if a subgrid is valid (no empty spots and no double numbers).

subgridValid :: Sudoku -> (Row,Column) -> Bool
subgridValid s (r,c) = (freeInSubgrid s (r,c)) == []

-- Returns if a sudoku is valid (no empty spots and no double numbers).

consistent :: Sudoku -> Bool
consistent s = foldr(\x acc -> (rowValid s x ) && (colValid s x) &&
               (foldr(\y acc -> (subgridValid s (x,y)) && acc) True [1..9]) &&
               acc) True [1..9]

-- EXTRA FUNCTIONS

-- Returns all the indices of the empty spots of a row, and therefore the column numbers.

openPosColumn :: Sudoku -> Row -> [(Value)]
openPosColumn s r = foldr (\x acc -> x + 1 : acc) [] (elemIndices 0 ((sud2grid s) !! (r - 1)))

-- Returns the top left point (x,y) in a subgrid.

topLeftPoint :: (Row, Column) -> (Row, Column)
topLeftPoint (r,c) = (x,y)
             where x = ((quot (r - 1) 3) * 3) + 1
                   y = ((quot (c - 1) 3) * 3) + 1

-- Returns all values of a subgrid.

getValues :: Sudoku -> (Row, Column) -> [Value]
getValues s (r,c) = foldr(\x acc -> (getValueAtPos s x) : acc) [] allPos
     where allPos = allPosSubgrid (r,c)

-- Returns the value of a certain position.

getValueAtPos :: Sudoku -> (Row, Column) -> Value
getValueAtPos s (r,c) = ((sud2grid s) !! (r - 1)) !! (c - 1)

-- Returns all positions of a subgrid.

allPosSubgrid :: (Row,Column) -> [(Row,Column)]
allPosSubgrid (r,c) = [ (rOther,cOther) | rOther<-y, cOther<-x ]
      where y = [r..r + 2]
            x = [c..c + 2]


