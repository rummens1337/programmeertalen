-- Namen: Thomas Vos, Michel Rummens
-- Studentnummers: 12829501, 13108093
-- This program solves a sudoku using several functions and a binary tree.
module SudokuSolver where

import Data.List
import Data.Maybe
import Sudoku

positions, values :: [Int]
positions = [1 .. 9]

values = [1 .. 9]

blocks :: [[Int]]
blocks = [[1..3], [4..6], [7..9]]

showDgt :: Value -> String
showDgt 0 = " "
showDgt d = show d

showSubgridRow :: [Value] -> String
showSubgridRow = unwords . map showDgt

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n ls = fst splitted : (chunksOf n . snd) splitted
  where
    splitted = splitAt n ls

showRow :: [Value] -> String
showRow sr =
  "| " ++ intercalate " | " (map showSubgridRow $ chunksOf 3 sr) ++ " |"

showGrid :: Grid -> String
showGrid grid =
  "+-------+-------+-------+\n" ++
  intercalate "\n+-------+-------+-------+\n" rows ++
  "\n+-------+-------+-------+"
  where
    rows = map (intercalate "\n") $ chunksOf 3 $ map showRow grid

sud2grid :: Sudoku -> Grid
sud2grid s = [[s (r, c) | c <- [1 .. 9]] | r <- [1 .. 9]]

grid2sud :: Grid -> Sudoku
grid2sud gr = \(r, c) -> pos gr (r, c)
  where
    pos :: [[a]] -> (Row, Column) -> a
    pos gr (r, c) = (gr !! (r - 1)) !! (c - 1)

printSudoku :: Sudoku -> IO ()
printSudoku = putStrLn . showGrid . sud2grid

-- Changes one value in the sudoku.
-- First picks the row containing the given coordinates, then splits that row
-- at the given coordinates. The head of the row (= value at given coordinates)
-- is then replaced with the new value, and lastly the sudoku is put back
-- together with the new value/row.
--
-- Sudoku: sudoku.
-- (Row, Column, Value): pair containing the row, column and new value.
-- Returns: the new sudoku.
extend :: Sudoku -> (Row, Column, Value) -> Sudoku
extend s (r, c, v) = grid2sud (init x2s ++ [init x1s ++ [v] ++ y1s] ++ y2s)
  where
    (x1s, y1s) = splitAt c (grid !! (r - 1))
    (x2s, y2s) = splitAt r grid
    grid = sud2grid s

-- Returns a list of every number available for one specific row.
--
-- Sudoku: sudoku.
-- Row: number of a row.
-- Returns: list of every number available for that one row
freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r = [1 .. 9] \\ (sud2grid s !! (r - 1))

-- Returns a list of every number available for one specific column.
--
-- Sudoku: sudoku.
-- Column: number of a column.
-- Returns: list of every number available for that one column.
freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = [1 .. 9] \\ map (\x -> x !! (c - 1)) (sud2grid s)

-- Returns a list of every number available for one specific subgrid.
--
-- Sudoku: sudoku.
-- (Row, Column): a pair of coordinates, which are located in a subgrid.
-- Returns: list of every number available for that one subgrid.
freeInSubgrid :: Sudoku -> (Row, Column) -> [Value]
freeInSubgrid s (r, c) = [1 .. 9] \\ getValues s (topLeftPoint (r, c))

-- Returns a list of every number available for one specific position.
--
-- Sudoku: sudoku.
-- (Row, Column): a pair of coordinates.
-- Returns: list of every number available for those coordinates.
freeAtPos :: Sudoku -> (Row, Column) -> [Value]
freeAtPos s (r, c) =
  ((freeInRow s r ++ freeInColumn s c ++ freeInSubgrid s (r, c)) \\ [1 .. 9])
  \\ [1 .. 9]

-- Returns a list of every open position.
--
-- Sudoku: sudoku.
-- Returns: a list of every open position.
openPositions :: Sudoku -> [(Row, Column)]
openPositions s = concatMap (\x -> zip [x,x ..] (openPosColumn s x)) [1 .. 9]

-- Returns if a row is valid (no empty spots and no double numbers).
--
-- Sudoku: sudoku.
-- Row: number of a row.
-- Returns: True if the row is valid, False if not.
rowValid :: Sudoku -> Row -> Bool
rowValid s r = null (freeInRow s r)

-- Returns if a column is valid (no empty spots and no double numbers).
--
-- Sudoku: sudoku.
-- Column: number of a column.
-- Returns: True if the column is valid, False if not.
colValid :: Sudoku -> Column -> Bool
colValid s c = null (freeInColumn s c)

-- Returns if a subgrid is valid (no empty spots and no double numbers).
--
-- Sudoku: sudoku.
-- (Row, Column): a pair of coordinates, which are located in a subgrid.
-- Returns: True if the subgrid is valid, False if not.
subgridValid :: Sudoku -> (Row, Column) -> Bool
subgridValid s (r, c) = null (freeInSubgrid s (r, c))

-- Returns if a sudoku is valid (no empty spots and no double numbers).
--
-- Sudoku: sudoku.
-- Returns: True if the sudoku is valid, False if not.
consistent :: Sudoku -> Bool
consistent s =
  foldr
    (\x acc ->
       rowValid s x &&
       colValid s x &&
       foldr (\y acc -> subgridValid s (x, y) && acc) True [1 .. 9] && acc)
    True
    [1 .. 9]

-- Helper function, changes a Maybe sudoku to a regular one so it can be
-- printed.
--
-- Maybe Sudoku: a sudoku, or nothing.
-- Returns: a sudoku, or the testsudoku if no solution has been found.
maybeToSud :: Maybe Sudoku -> Sudoku
maybeToSud = fromMaybe testSudoku

-- Returns a list of all contraints, which is the tree.
--
-- Sudoku: sudoku.
-- Returns: a sorted list of all constraints of that sudoku.
constraints :: Sudoku -> [Constraint]
constraints s =
  customSort (map (\x -> (fst x, snd x, freeAtPos s x)) (openPositions s))

-- Solves the sudoku, returns a solved sudoku if a solution has been found,
-- otherwise nothing.
--
-- Sudoku: unsolved sudoku.
-- Returns: the solved sudoku, or nothing if there is no solution.
solveSudoku :: Sudoku -> Maybe Sudoku
solveSudoku s =
  case finalNode of
    Just finalNode -> Just (fst finalNode)
    Nothing -> Nothing
  where
    finalNode = addNodeOrNot s (constraints s)

-- Adds all values from constraints with only one possibility to the sudoku.
--
-- Sudoku: sudoku.
-- Returns: the sudoku with all the values from those constraints added.
addValues :: Sudoku -> Sudoku
addValues s = foldr (flip extend) s posValues
  where
    posValues =
      map
        (\c -> (firstElement c, secondElement c, head (thirdElement c)))
        (takeFirstConstraints s)

-- Updates the constraint list (always used after addValues).
--
-- Sudoku: sudoku.
-- Returns: the updated constraint list.
remConstValues :: Sudoku -> [Constraint]
remConstValues s =
  customSort (foldr remOneValue allConstraints firstConstraints)
  where
    firstConstraints = takeFirstConstraints s
    allConstraints = constraints s

-- Removes a value from every constraint in the same row, column or subgrid as
-- the given constraint, and removes the given constraint itself too.
--
-- Constraint: constraint that was used.
-- [Constraint]: list of all constraints.
-- Returns: the updated constraint list, for one value.
remOneValue :: Constraint -> [Constraint] -> [Constraint]
remOneValue c t =
  delete
    c
    (foldr
       (\x acc ->
          if x == c
            then x : acc
            else if containsValue x c
                   then getNewConstraint x : acc
                   else x : acc)
       []
       t)

-- Takes the first constraints from the list
-- (the ones with only one possibility).
--
-- Sudoku: sudoku.
-- Returns: a list of the first constraints of that sudoku.
takeFirstConstraints :: Sudoku -> [Constraint]
takeFirstConstraints s =
  takeWhile (\x -> length (thirdElement x) == 1) (constraints s)

-- Does 4 things: end the search of a node if no solution has been found,
-- return a solution if one has been found, do constraints with 1 possibility
-- and make a new node if the first constraint of the list has 2 or more
-- possibilities.
--
-- Sudoku: sudoku.
-- [Constraint]: the constraint list of that sudoku.
-- Returns: a new node, or nothing.
addNodeOrNot :: Sudoku -> [Constraint] -> Maybe Node
addNodeOrNot oldSudoku oldConstraints
  | not (not (null oldConstraints) || consistent oldSudoku) = Nothing
  | null oldConstraints && consistent oldSudoku =
    Just (oldSudoku, oldConstraints)
  | length listValues == 1 = addNodeOrNot newSudoku newConstraints
  | otherwise = solveNode oldSudoku oldConstraints
  where
    newSudoku = addValues oldSudoku
    newConstraints = remConstValues oldSudoku
    listValues = thirdElement (head oldConstraints)

-- Solves a node. Calls addNodeOrNot with a single value from the first
-- constraint, and recursively calls itself again if that value did not yield
-- a solution. If a solution is found, it returns the solution.
--
-- Sudoku: sudoku.
-- [Constraint]: the constraint list of that sudoku.
-- Returns, a new node, or nothing.
solveNode :: Sudoku -> [Constraint] -> Maybe Node
solveNode s c =
  case newNode of
    Just (s, c) -> Just (s, c)
    Nothing -> solveNode s newlistT
  where
    const = head c
    headConstraint = (firstElement const, secondElement const, [head values])
    tailConstraint = (firstElement const, secondElement const, tail values)
    newListH = headConstraint : tail c
    newlistT = tailConstraint : tail c
    newNode = addNodeOrNot s newListH

-- EXTRA FUNCTIONS -- The small helper functions can be found here.

-- Returns all the indices of the empty spots of a row -> column numbers.
--
-- Sudoku: sudoku.
-- Row: number of a row.
-- Returns: all the indices of the empty spots of a row -> column numbers.
openPosColumn :: Sudoku -> Row -> [Value]
openPosColumn s r = map (+ 1) (elemIndices 0 (sud2grid s !! (r - 1)))

-- Returns the top left point (x,y) in a subgrid.
--
-- (Row, Column): pair of coordinates in a subgrid.
-- Returns: the top left coordinates of that subgrid.
topLeftPoint :: (Row, Column) -> (Row, Column)
topLeftPoint (r, c) = (x, y)
  where
    x = (quot (r - 1) 3 * 3) + 1
    y = (quot (c - 1) 3 * 3) + 1

-- Returns all values of a subgrid.
--
-- Sudoku: sudoku.
-- (Row, Column): pair of coordinates in a subgrid.
-- Returns: all values of a subgrid.
getValues :: Sudoku -> (Row, Column) -> [Value]
getValues s (r, c) = map (getValueAtPos s) allPos
  where
    allPos = allPosSubgrid (r, c)

-- Returns the value of a certain position.
--
-- Sudoku: sudoku.
-- (Row, Column): pair of coordinates.
-- Returns: a value at that position in the Sudoku.
getValueAtPos :: Sudoku -> (Row, Column) -> Value
getValueAtPos s (r, c) = (sud2grid s !! (r - 1)) !! (c - 1)

-- Returns all positions of a subgrid.
--
-- (Row, Column): pair of coordinates in a subgrid.
-- Returns: all coordinates in that subgrid.
allPosSubgrid :: (Row, Column) -> [(Row, Column)]
allPosSubgrid (r, c) = [(rOther, cOther) | rOther <- y, cOther <- x]
  where
    y = [r .. r + 2]
    x = [c .. c + 2]

-- Sorts the constraints based on the amount of possibilities.
--
-- [Constraint]: list of constraints.
-- Returns: the sorted list of constraints.
customSort :: [Constraint] -> [Constraint]
customSort = sortBy sortLT

-- Determines how to sort the list of constraints (based on the amount of
-- possibilities).
--
-- (r1, c1, v1): first tuple.
-- (r2, c2, v2): second tuple.
-- Returns: Less Than, Greater Than or Equal.
sortLT (r1, c1, v1) (r2, c2, v2)
  | length v1 < length v2 = LT
  | length v1 > length v2 = GT
  | length v1 == length v2 = EQ

-- Gets the first element of a tuple.
--
-- (x, _, _): a tuple.
-- Returns: the first element of that tuple.
firstElement (x, _, _) = x

-- Gets the second element of a tuple.
--
-- (_, y, _): a tuple.
-- Returns: the second element of that tuple.
secondElement (_, y, _) = y

-- Gets the third element of a tuple.
--
-- (_, _, z): a tuple.
-- Returns: the third element of that tuple.
thirdElement (_, _, v) = v

-- Returns true if the given constraints are about a position in the same row,
-- column or subgrid.
--
-- Constraint: constraint to be compared with.
-- Constraint: constraint from the list of constraints.
-- Returns: True if they are about a position in the same row, column
-- or subgrid.
containsValue :: Constraint -> Constraint -> Bool
containsValue c1 c2 =
  firstElement c1 == firstElement c2 ||
  secondElement c1 == secondElement c2 ||
  topLeftPoint (firstElement c1, secondElement c1) ==
  topLeftPoint (firstElement c2, secondElement c2)

-- Returns a new constraint, with a value removed.
--
-- Constraint: a constraint from the list.
-- Returns: a new constraint, with a value deleted.
getNewConstraint :: Constraint -> Constraint
getNewConstraint c =
  ( firstElement c
  , secondElement c
  , delete (head (thirdElement c)) (thirdElement c))

-- A testgrid, used for quickly testing the stage 1 and 2 functions.
--
-- Returns: the testgrid.
testGrid :: Grid
testGrid =
  [ [0,0,3,0,0,0,0,0,9]
  , [0,5,0,0,8,0,1,0,3]
  , [0,0,9,0,0,3,0,5,0]
  , [0,0,1,6,0,4,0,9,0]
  , [8,0,5,9,0,2,3,6,4]
  , [0,0,0,5,0,8,2,0,7]
  , [3,0,7,0,6,5,9,4,8]
  , [0,0,2,0,0,7,6,0,1]
  , [0,0,8,0,0,0,0,0,0]
  ]

-- A testsudoku, used to display the testgrid shown above.
--
-- Returns: the testgrid, as sudoku.
testSudoku :: Sudoku
testSudoku = grid2sud testGrid
