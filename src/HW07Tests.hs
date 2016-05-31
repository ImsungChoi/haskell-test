module HW07Tests where

import HW07
import Testing
import Data.List

-- Exercise 2 -----------------------------------------

ex1Tests :: [Test]
ex1Tests = [ testF2 "liftM test" liftM
            [((+1), Just 5, Just 6)]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = []
 
 
-- Exercise 3 -----------------------------------------          

ex3Tests :: [Test]
ex3Tests = []

-- Exercise 4 ----------------------------------------- 

ex4Tests :: [Test]
ex4Tests = [] 
           
           
-- Exercise 5 ----------------------------------------- 

ex5Tests :: [Test]
ex5Tests = [] 

-- Exercise 6 ----------------------------------------- 

ex6Tests :: [Test]
ex6Tests = [] 

-- Exercise 7 ----------------------------------------- 

ex7Tests :: [Test]
ex7Tests = []

-- Exercise 9 ----------------------------------------- 

ex9Tests :: [Test]
ex9Tests = []

-- All Tests -------------------------------------------

allTests :: [Test]
allTests =  ex2Tests -- ++ ex3Tests ++ ex4Tests ++ ex5Tests ++ ex6Tests ++ ex7Tests ++ ex9Tests