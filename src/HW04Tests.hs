module HW04Tests where

import HW04
import Testing
import Data.List

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ testF2 "equal test" (==)
            [(P [1, 2, 3, 4], P [1, 2, 3, 4], True)]
           ]
 
 
-- Exercise 3 -----------------------------------------          

ex3Tests :: [Test]
ex3Tests = [ testF1 "show test" show
            [(P [1, 0, 0, 2], "2x^3 + 1"),
             (P [0, -1, 2], "2x^2 + -x")]
           ]

-- Exercise 4 ----------------------------------------- 

ex4Tests :: [Test]
ex4Tests = [ testF2 "plus test" plus
            [(P [5, 0, 1], P [1, 1, 2], P [6, 1, 3]),
             (P [1, 0, 1], P [1, 1], P [2, 1, 1])]
           ] 
           
           
-- Exercise 5 ----------------------------------------- 

ex5Tests :: [Test]
ex5Tests = [ testF2 "times test" times
            [(P [1, 1, 1], P [2, 2], P [2, 4, 4, 2])]
           ] 

-- Exercise 6 ----------------------------------------- 

ex6Tests :: [Test]
ex6Tests = [ testF1 "negate test" negate
            [(P [1, 1, 1], P [-1, -1, -1])],
             testF1 "fromInteger test" fromInteger
             [(3, P [3])]
           ] 

-- Exercise 7 ----------------------------------------- 

ex7Tests :: [Test]
ex7Tests = [ testF2 "applyP test" applyP
            [(P [1, 2, 1], 1, 4),
             (P [1, 2, 1], 2, 9)]
           ]

-- Exercise 9 ----------------------------------------- 

ex9Tests :: [Test]
ex9Tests = [ testF1 "deriv test" deriv
            [(P [5, 3, 1], P [3, 2])]
           ]

-- All Tests -------------------------------------------

allTests :: [Test]
allTests =  ex2Tests ++ ex3Tests ++ ex4Tests ++ ex5Tests ++ ex6Tests ++ ex7Tests ++ ex9Tests