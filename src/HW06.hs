{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor()

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2) 

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) (tail fibs2) fibs2

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a as) = a : streamToList as

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons a as) = Cons (f a) (f <$> as)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat a = Cons a (sRepeat a)

sIterate :: (a -> a) -> a -> Stream a
sIterate f a = Cons a (sIterate f $ f a)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons a as) bs = Cons a (sInterleave bs as)

sTake :: Int -> Stream a -> [a]
sTake n a = take n $ streamToList a

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = helper 0
    where helper n = sInterleave (sRepeat n) (helper $ n+1)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = sIterate f seed
    where f i = ((1103515245 * i) + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax a  = Just (strictMinMax (head a) (head a) $ tail a)
    where strictMinMax :: Int -> Int -> [Int] -> (Int, Int)
          strictMinMax m1 m2 []     = (m1, m2)
          strictMinMax m1 m2 (x:xs) = m1 `seq` m2 `seq` strictMinMax (min m1 x) (max m2 x) xs


-- Exercise 10 ----------------------------------------

data Matrix = M Integer Integer Integer Integer

instance Num Matrix where
    (*) (M a11 a12 a21 a22) (M b11 b12 b21 b22) = M m11 m12 m21 m22
        where m11 = a11 * b11 + a12 * b21
              m12 = a11 * b12 + a12 * b22
              m21 = a21 * b11 + a22 * b21
              m22 = a21 * b12 + a22 * b22
    (+) = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined
    negate = undefined

m1 :: Matrix
m1 = M 1 1 1 0

getFirst :: Matrix -> Integer
getFirst (M o _ _ _) = o

fastFib :: Int -> Integer
fastFib n
    | n == 0    = 1
    | otherwise = getFirst $ m1^(n)