import Data.List
import Test.QuickCheck  

--Instead of discarding tests, make good n's out of the bad ones
naturalize :: Int -> Int
naturalize n = abs n -- If it was negative it will turn positive, becoming a useful n

testProperty :: Int -> Bool
testProperty n = length( subsequences [1..n]) == 2^n

secondExercise :: [Int] -> Bool
secondExercise = \xs -> (all testProperty (map naturalize xs))

main                = quickCheck (secondExercise)

-- The property is hard to test because for a big integer n, the subsequence [1..n] is gigantic (2^n in length accordingly to the statement), 
--		thus it takes time to make the subsequence list or even count its length making the property hard to test, 
--		or even only computationally possible to do it for relatively small n's.
-- Answering to the follow-up question of exercise 2, I am not only testing if the property holds for all natural n, but testing if both the property
--		and the subsequences function are correct. If the tests fail I can not say anything about the subsequences or the property individually, I can
-- 		only say that they fail together, whether because the subsequences function is not correctly implemented or the property is not true for all naturals.
-- time: 10 minutes