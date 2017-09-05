import Data.List
import Test.QuickCheck  

factorial :: Int -> Int
factorial 0 = 1
factorial n = n* factorial (n-1)

--Instead of discarding tests, make good n's out of the bad ones
naturalize :: Int -> Int
naturalize n = abs n -- If it was negative it will turn positive, becoming a useful n

-- I am testing if the number of permutations of [1..n] can be n!
testProperty :: Int -> Bool
testProperty n = length( permutations [1..n]) == factorial n

thirdExercise :: [Int] -> Bool
thirdExercise = \xs -> (all testProperty (map naturalize xs))

main                = quickCheck (thirdExercise)

-- The property is hard to test because calculating the number of permutations of a set of length n is O(n!), 
-- 		being computationally infeasible to calculate it for big n's
-- In fact I am not only testing if the property is correct for every natural n, 
--		i'm testing the whole system of using my own factorial, using the function "permutations" and checking if the property holds.
-- 		This means I can't say anything about each individual part of the system, I can only say things about the whole.
--		If the tests failed, it could be because my factorial is not correctly implemented, the permutations function is not correctly implemented,
--			or the property is not true for all natural n, but I couldn't be sure of either.
-- time: 7 minutes