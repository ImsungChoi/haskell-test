module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Exercise 2 -----------------------------------------
toRevDigits :: Integer -> [Integer]
toRevDigits x
    | x <= 0    = []
    | otherwise = lastDigit x : toRevDigits (dropLastDigit x)

-- Exercise 3 -----------------------------------------

-- Double every first number in a list starting on the right.
doubleEveryFirst :: [Integer] -> [Integer]
doubleEveryFirst []         = []
doubleEveryFirst (x:[])     = [x]
doubleEveryFirst (x:(y:zs)) = x*2 : y : doubleEveryFirst zs 

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryFirst (reverse x))

-- Exercise 4 -----------------------------------------

-- Calculate the sum of two digits 
sumDigit :: Integer -> Integer
sumDigit x = sum (toRevDigits x)

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits []        = 0
sumDigits (x:xs)    = sumDigit x + sumDigits xs 

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = lastDigit (sumDigits (doubleEveryOther (toRevDigits x))) == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n == 1    = [(a, c)]
    | otherwise = hanoi (n-1) a b c
               ++ hanoi 1 a c b 
               ++ hanoi (n-1) c a b


-- Exercise 7 -----------------------------------------

-- Towers of Hanoi for four pegs
hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 n a b c d
    | n <= 0        = []
    | n == 1        = [(a, d)]
    | n == 2        = [(a, c), (a, d), (c, d)]
    | otherwise     = hanoi2 k a c d b 
                    ++ hanoi (n-k) a c d 
                    ++ hanoi2 k b a c d
        where
        k = n - fromIntegral (round (sqrt (fromIntegral (2*n + 1)))) + 1
