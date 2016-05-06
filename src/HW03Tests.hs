module HW03Tests where

import HW03
import Testing
import Data.List

-- Exercise 1 -----------------------------------------

s1 x = 5
s2 = extend s1 "s2" 6
s3 = extend s2 "s3" 7

ex1Tests :: [Test]
ex1Tests = [ testF1 "extend test" s2 
            [("s2", 6), ("s1", 5)],
             testF1 "empty test" empty
             [("s2", 0), ("s1", 0)],
             testF1 "extend test" s3
             [("s3", 7), ("s2", 6), ("s1", 5)]
           ]


-- Exercise 2 -----------------------------------------
e1 = Op (Val 1) Eql (Val 2)
r1 = evalE empty e1

e2 = Val 5
r2 = 5

e3 = Op (Val 3) Gt (Var "x")
r3 = evalE (extend empty "x" 2) e3

ex2Tests :: [Test]
ex2Tests = [ testF1 "evalE test" id
            [(0, r1)],
             testF1 "evalE test" id
            [(5, r2)],
             testF1 "evalE test" id
            [(1, r3)]
           ]

-- Programs --------------------------------------------
factorialState = run (extend empty "In" 4) factorial
sqaureRootState = run (extend empty "A" 9) squareRoot
fibonacciState = run (extend empty "In" 6) fibonacci
test1 :: Statement
test1 = Sequence (Assign "Out" (Val 5)) (Assign "Out" (Val 6))
testWrapper1 = run empty test1

test2 :: Statement
test2 = While (Op (Var "x") Gt (Val 4)) (Sequence (Assign "y" (Op (Var "y") Times (Var "x"))) (Assign "x" (Op (Var "x") Minus (Val 1))))
testWrapper2 = run (extend (extend empty "y" 1) "x" 1) test2

test3 :: Statement
test3 = If (Op (Var "x") Gt (Val 2)) (Assign "x" (Val 4)) (Assign "x" (Val 5))
testWrapper3 = run (extend empty "x" 1) test3

programTests :: [Test]
programTests = [ testF1 "factorial test" factorialState
                [("Out", 24)],
                 testF1 "test1 test" testWrapper1
                [("Out", 6)],
                 testF1 "test2 test" testWrapper2
                [("y", 1)],
                 testF1 "test3 test" testWrapper3
                [("x", 5)],
                 testF1 "sqaureRoot test" sqaureRootState
                [("B", 3)],
                 testF1 "fibonacci test" fibonacciState
                 [("Out", 13)]
               ]

-- All Tests -------------------------------------------

allTests :: [Test]
allTests =  ex1Tests 
            ++ ex2Tests
            ++ programTests