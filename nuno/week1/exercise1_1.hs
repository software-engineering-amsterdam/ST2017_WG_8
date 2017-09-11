module Lab1 where
import Data.List
import Test.QuickCheck  

--Instead of discarding tests, make good n's out of the bad ones
naturalize :: Int -> Int
naturalize n = abs n -- If it was negative it will turn positive, becoming a useful n

square :: Int -> Int
square n = n*n

--Make the sum by adding the square of each natural k (1 <= k <= n) at a time
sumIndividual :: Int -> Int
sumIndividual n = sum (map square [1..n])

--Apply the formula defended by the statement of exercise 2
sumTogether :: Int -> Int
sumTogether n = div (n*(n+1)*(2*n+1)) 6

-- goodSum checks if doing the sums each number at a time is the same as applying the formula, which is what the statement defends
goodSum :: Int -> Bool
goodSum n = sumIndividual n == sumTogether n

-- The test is basically applying the goodSum function for each n given by quickCheck.
-- I make every n positive with (map naturalize xs) so every n is useful for testing
firstExercise :: [Int] -> Bool
firstExercise = \xs -> (all goodSum (map naturalize xs))

main                = quickCheck (firstExercise)

-- time: 3 hours (it includes the learning how to program in haskell part)