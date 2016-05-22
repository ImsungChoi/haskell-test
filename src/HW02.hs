{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List
import Data.Ord

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
          addCode c = [nc : c | nc <- colors]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve x = if last s == last (init s)
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
fiveGuess x = if last s == last (init s)
              then init s
              else s
    where s = solveHelper' x (allCodes 4) [Red, Red, Green, Green]

solveHelper' :: Code -> [Code] -> Code -> [Move]
solveHelper' x y g
    | length y == 1 = [move]
    | otherwise     = move : solveHelper' x filtered (guess filtered)
        where move      = getMove x g
              filtered  = filterCodes move y
            
guess :: [Code] -> Code
guess x = guessHelper x (getHits x) 
    where guessHelper :: [Code] -> [Int] -> Code
          guessHelper xs hs = fst (minimumBy (comparing snd) (zip xs hs))

possible :: [(Int, Int)]
possible = [(x, y) | x <- [0..4], y <- [0..4], x + y <= 4]

getHits :: [Code] -> [Int]
getHits xs = map (`hitHelper` xs) xs
    where hitHelper :: Code -> [Code] -> Int
          hitHelper a b = calMax $ getMoves a b

getMoves :: Code -> [Code] -> [Move]
getMoves x = map (`getMove` x)

calMax :: [Move] -> Int
calMax ms = maximum (map (`calPossible` ms) possible)
    where calPossible :: (Int, Int) -> [Move] -> Int
          calPossible p (m:ms')
            | null ms'  = 0
            | match p m = 1 + calPossible p ms'
            | otherwise = calPossible p ms'

-- getHit :: Code -> Code -> Int
-- getHit x y = (getMove x y)
--     where return :: Move -> (Int, Int)
--           return (Move _ a b) = (a , b)
          
match :: (Int, Int) -> Move -> Bool
match (a, b) (Move _ x y) 
    | a == x && b == y  = True
    | otherwise         = False 

testFiveGuess :: [Code] -> [Int]
testFiveGuess = map (length . fiveGuess)