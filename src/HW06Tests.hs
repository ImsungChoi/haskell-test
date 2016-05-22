module HW06Tests where

import HW06
import Testing
import Data.List

-- Exercise 1 & 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [testF2 "fibo test" (==)
            [(take 10 fibs1, take 10 fibs2, True)]
           ]
 
 
-- Exercise 3 -----------------------------------------          

ex3Tests :: [Test]
ex3Tests = []

-- Exercise 4 ----------------------------------------- 

ex4Tests :: [Test]
ex4Tests = [] 
           
           
-- Exercise 5 ----------------------------------------- 

ex5Tests :: [Test]
ex5Tests = []

-- All Tests -------------------------------------------

allTests :: [Test]
allTests =  ex2Tests ++ ex3Tests ++ ex4Tests ++ ex5Tests