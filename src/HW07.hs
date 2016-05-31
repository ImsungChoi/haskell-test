{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do
    a <- ma
    return $ f a

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i1 i2 v = do
    v1 <- v !? i1
    v2 <- v !? i2
    return $ v // [(i1, v2), (i2, v1)]

swapV' :: Int -> Int -> Vector a -> Vector a
swapV' i1 i2 v = v // [(i1, v2), (i2, v1)]
  where v1 = v ! i1
        v2 = v ! i2
        
-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as = sequence $ map f as

getElts :: [Int] -> Vector a -> Maybe [a]
getElts as v = mapM (v !?) as 

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = (v !?) `liftM` getRandomR (0, length v - 1) 
                 
-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.fromList `liftM` replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n t = V.fromList `liftM` replicateM n (getRandomR t)

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = (v //) `liftM` go v (getN $ length v)
  where go :: Vector a -> [Int] -> Rnd ([(Int, a)])
        go v' as = (zip' v' as) `liftM` mapM (\x -> getRandomR $ getPair x) as

zip' :: Vector a -> [Int] -> [Int] -> [(Int, a)]
zip' _ [] []          = []
zip' _ [] (_:_)       = []
zip' _ (_:_) []       = []
zip' v (a:as) (b:bs)  = (a, v!b) : (b, v!a) : zip' (swapV' a b v) as bs
   
getPair :: Int -> (Int, Int)
getPair n = (0, n - 1) 

getN :: Int -> [Int]
getN n = reverse [1 .. n-1];

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v p = (mfilter (pivot <) v', pivot, mfilter (pivot >=) v')
  where pivot = v!p
        v' = V.take p v V.++ V.drop (p+1) v

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v 
  | v == V.empty = V.empty 
  | otherwise    = qsort [y | y <- xs, y < x]
                   <> (x `V.cons` qsort [y | y <- xs, y >= x])
    where x = V.head v
          xs = V.drop 1 v

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  | v == V.empty = V.empty
  | otherwise    = partitionAt v rp
    where rp = getRandomR (0, length v -1) 

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select = undefined

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = undefined

newDeck :: Rnd Deck
newDeck =  undefined

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard = undefined

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = undefined

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main' :: IO ()
main' = evalRandIO newDeck >>= repl . State 100