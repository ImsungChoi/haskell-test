{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1, 0]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (P b) == (P c) = b == c 
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P b) = foldr1 (\ t ts -> t ++ " + " ++ ts) m
        where m = filter (/= "") (map f z)
              f :: (String, Int) -> String
              f ("0", _)    = []
              f (i, 0)      = i
              f ("-1", 1)   = "-x"
              f ("1", 1)    = "x"
              f ("-1", j)   = "-x^" ++ show j
              f ("1", j)    = "x^" ++ show j
              f (i, j)      = i ++ "x^" ++ show j
              z = zip (reverse $ map show b) (reverse [0..length b - 1])

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P b1) (P b2) = if length b1 >= length b2
                     then P $ zipWith (+) b1 (b2 ++ repeat 0)
                     else plus (P b2) (P b1)

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P b1) (P b2) = P (zipWith (*) b1 b2)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P b)    = P (map negate b)
    fromInteger i   = P [fromInteger i]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined
