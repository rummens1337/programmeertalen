-- Namen: Thomas Vos, Michel Rummens
-- Studentnummers: 12829501, 13108093
-- This program can solve a sudoku using several functions and a binary tree.

module SudokuSolver where

import Sudoku
import Data.List
import Data.Maybe

testGrid :: Grid -- Testgrid, weghalen voor inleveren
testGrid =
  [ [1,1,1,0,0,0,7,0,9]
  , [0,0,0,7,8,9,1,2,3] -- 4,5,6
  , [7,8,9,1,2,3,4,5,6]
  , [2,3,1,6,7,4,8,9,5]
  , [8,7,5,9,1,2,0,6,4]
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

-- [1..9] \\ [row ofzo] (list difference function, yoinked waardes die overeenkomen uit left array)

-- step 1

extend :: Sudoku -> (Row,Column,Value) -> Sudoku -- WERKT NOG NIET
extend s (r,c,v) = doe iets lol

freeInRow :: Sudoku -> Row -> [Value] -- Werkt.
freeInRow s r = [1..9] \\ ((sud2grid s) !! (r - 1))

freeInColumn :: Sudoku -> Column -> [Value] -- Werkt.
freeInColumn s c = [1..9] \\ (foldr(\x acc -> x !! (c - 1) : acc) [] (sud2grid s))

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value] -- WERKT NOG NIET, wss tuple met fst en snd uit
freeInSubgrid s (r,c) = [1..9] \\ (quot r 3, quot c 3)

freeAtPos :: Sudoku -> (Row,Column) -> [Value] -- Werkt wss.
freeAtPos s (r,c) = [1..9] \\ ((freeInRow s r) ++ (freeInColumn s c) ++ (freeInSubgrid s (r,c)))

openPositions :: Sudoku -> [(Row,Column)] -- Werkt.
openPositions s = concat (foldr(\x acc -> zip [x,x..] (openPosColumn s x) : acc) [] [1..9])

-- step 2

rowValid :: Sudoku -> Row -> Bool -- Werkt.
rowValid s r = (freeInRow s r) == []

colValid :: Sudoku -> Column -> Bool -- Werkt.
colValid s c = (freeInColumn s c) == []

subgridValid :: Sudoku -> (Row,Column) -> Bool -- Werkt wss.
subgridValid s (r,c) = (freeInSubgrid s (r,c)) == []

consistent :: Sudoku -> Bool -- Werkt theoretisch gezien wel, nog niet kunnen testen (WERKT DUS WSS NOG NIET)
consistent s = foldr(\x acc -> (rowValid x (sud2grid s)) && (columnValid x (sud2grid s)) && -- Checkt of alle rows en columns valid zijn.
               (foldr(\y acc -> (subgridValid (x,y) s) && acc) True [1..9]) && -- Checkt of alle subgrids valid zijn.
               acc) True [1..9]

-- Extra functies

-- Returns all the indices of the empty spots of a row, and therefore the column numbers.

openPosColumn :: Sudoku -> Row -> [(Value)]
openPosColumn s r = foldr (\x acc -> x + 1 : acc) [] (elemIndices 0 ((sud2grid s) !! (r - 1)))

-- Step 1 - Create list with empty positions in the given sudoku
--extend :: Sudoku -> (Row,Column,Value) -> Sudoku
--freeInRow :: Sudoku -> Row -> [Value]
--freeInColumn :: Sudoku -> Column -> [Value]
--freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
--freeAtPos :: Sudoku -> (Row,Column) -> [Value]
--openPositions :: Sudoku -> [(Row,Column)]

-- Step 2 - Check if sudoku is valid
--rowValid :: Sudoku -> Row -> Bool Hergebruik freeInRowfuncties!
--colValid :: Sudoku -> Column -> Bool
--subgridValid :: Sudoku -> (Row,Column) -> Bool
--consistent :: Sudoku -> Bool
