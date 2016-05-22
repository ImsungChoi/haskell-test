{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
-- exactMatches :: Code -> Code -> Int
-- exactMatches [] []          = 0
-- exactMatches (x:xs) (y:ys) 
--                 | x == y    = 1 + exactMatches xs ys
--                 | otherwise = exactMatches xs ys 

exactMatches :: Code -> Code -> Int
exactMatches x y =  length (filter id (zipWith (==) x y))

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
-- countColors :: Code -> [Int]  
-- countColors pegs = go colors pegs
--     where go :: Code -> Code -> [Int]
--           go [] _         = []
--           go (c:cs) y     = countColor c y : go cs y

-- countColor :: Peg -> Code -> Int
-- countColor _ []     = 0
-- countColor x (y:ys)
--         | x == y    = 1 + countColor x ys
--         | otherwise = countColor x ys

countColors :: Code -> [Int]
countColors pegs = map (`count` pegs) colors
    where count :: Peg -> Code -> Int
          count p cs = length (filter id (map (== p) cs))

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches x y = sum (zipWith min (countColors x) (countColors y))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove x y = Move y em om
    where em = exactMatches x y
          om = (-em) + matches x y 


-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move s a b) c = a == a' && b == b'
    where (Move _ a' b') = getMove s c 

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m = filter (isConsistent m)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes x = go x [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]]
    where go :: Int -> [Code] -> [Code]
          go 1 acc = acc
          go i acc = go (i-1) (concatMap addCode acc)
          addCode :: Code -> [Code]
          addCode c = [Red : c] ++ [Green : c] ++ [Blue : c] ++
                      [Yellow : c] ++ [Orange : c] ++ [Purple : c]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve x = if last s == (last $ init s)
          then init s
          else s
    where s = solveHelper x (allCodes 4)

solveHelper :: Code -> [Code] -> [Move]
solveHelper x y
    | length y == 1 = [move]
    | otherwise     = move : solveHelper x fc 
        where move = getMove x (head y)
              fc   = filterCodes move y

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess x = if last s == (last $ init s)
              then init s
              else s
    where s = solveHelper' [Red, Red, Green, Green] (allCodes 4)

solveHelper' :: Code -> [Code] -> [Move]
solveHelper' x y
    | length y == 1 = [move]
    | otherwise     = move : solveHelper' gd filtered
        where move      = getMove x gd
              filtered  = filterCodes move y
              gd        = guess filtered
            
guess :: [Code] -> Code
guess x = guessHelper x (getHits x) 
    where guessHelper :: [Code] -> [Int] -> Code
          guessHelper x y = fst (minimumBy (\c1 c2 -> compare (snd c1) (snd c2)) (zip x y))

possible :: [(Int, Int)]
possible = [(0, 0), (0, 1), (0, 2), (0, 3), 
            (0, 4), (1, 0), (1, 1), (1, 2), 
            (1, 3), (2, 0), (2, 1), (2, 2), 
            (3, 0), (3, 1), (4, 0)
           ];

getHits :: [Code] -> [Int]
getHits xs = map (\x -> hitHelper x xs) xs
    where hitHelper :: Code -> [Code] -> Int
          hitHelper a b = calMax $ getMoves a b

getMoves :: Code -> [Code] -> [Move]
getMoves x ys = map (\y -> getMove y x) ys

calMax :: [Move] -> Int
calMax ms = maximum (map (\p -> calPossible p ms) possible)
    where calPossible :: (Int, Int) -> [Move] -> Int
          calPossible p (m:ms')
            | length ms' == 0   = 0
            | match p m         = 1 + calPossible p ms'
            | otherwise         = calPossible p ms'

-- getHit :: Code -> Code -> Int
-- getHit x y = (getMove x y)
--     where return :: Move -> (Int, Int)
--           return (Move _ a b) = (a , b)
          
match :: (Int, Int) -> Move -> Bool
match (a, b) (Move _ x y) 
    | a == x && b == y  = True
    | otherwise         = False 