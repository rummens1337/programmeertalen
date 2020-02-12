module Puzzles where

-- 1 : Length
length' l = foldr (\_ n -> 1 + n) 0 l

-- OLD
--length' :: [Integer] -> Integer
--length' [] = 0
--length' (x:xs) = 1 + length' xs

-- 2 : Or
or' r = foldr (||) False r

-- OLD
--or' :: [Bool] -> Bool
--or' [] = False
--or' (x:xs)
--    | x == True = True
--    | otherwise = or' xs

-- 3 : Elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = foldr (||) False (map (x ==) xs)

-- OLD
--elem' :: Integer -> [Integer] -> Bool
--elem' n [] = False
--elem' n (x:xs)
--    | n == x = True
--    | otherwise = elem' n xs

-- 4 : Map
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:).f) []

-- OLD
--map' :: (a -> b) -> [a] -> [b]
--map' f [] = []
--map' f (x:xs) = f x : (map' f xs)

-- 5 : Plusplus
plusplus :: [a]->[a]->[a]
plusplus xs ys = foldr (\x l1 -> x:l1) ys xs

-- OLD
--plusplus :: [String] -> String
--plusplus [] = ""
--plusplus (x:xs) = x ++ plusplus xs

-- 6 : ReverseR
reverseR reverser = foldr (\b g x -> g (b : x)) id reverser []


-- OLD
--reverseR :: [String] -> [String]
--reverseR [] = []
--reverseR (x:xs) = xs

---- 7 : ReverseL
reverseL list = foldl (flip (++)) [] (map (\x -> [x]) list)

--OLD
--ReverseL :: [String] -> [String]
--ReverseL [] = []
--ReverseL (x:xs) = tail xs

-- 8 : isPalindrome
isPalindrome w = w == reverse w

-- 9 : Fibonacci
fibonacci = 0 : scanl (+) 1 fibonacci

-- OLD - returns fib number in list position of given Integer
--fibonacci :: Integer -> Integer
--fibonacci x
--  | x < 2 = 1
--  | otherwise = 0 + 1 + fibonacci (x - 1) + fibonacci (x - 2)


-- Personal tests
add :: Integer -> Integer -> Integer
add a b = a + b

foo :: Integer -> Integer
foo 0 = 1
foo n = n * foo (n-1)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

boomBangs xs = [ if x == True then "BOOM!" else "BANG!" | x <- xs]

--elem' :: Integer -> [Integer] -> Boolean
--elem' = Eq a => a -> t a -> Bool
