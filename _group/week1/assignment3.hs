module Lab1 where
import Data.List
import Test.QuickCheck

-- Assignment 3 #Time: 7 mins

-- The property is hard to test because calculating the number of permutations of a set of length n is O(n!), 
-- being computationally infeasible to calculate it for big n's
-- In fact I am not only testing if the property is correct for every natural n, 
-- i'm testing the whole system of using my own factorial, using the function "permutations" and checking if the property holds.
-- This means I can't say anything about each individual part of the system, I can only say things about the whole.
-- If the tests failed, it could be because my factorial is not correctly implemented, the permutations function is not correctly implemented,
-- or the property is not true for all natural n, but I couldn't be sure of either.

perm_test = quickCheck (\n -> length (permutations [1..n]) == product [1..n])
