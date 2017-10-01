module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

{-
Exercise 1 (30 mins)
Use a foldl to get trough the list and just check in which quartile the current list element belongs, and add one count to the corresponding result.
The test results show that 'Red Curry' his statement is correct.
-}
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
          p <- getStdRandom random
          ps <- probs (n-1)
          return (p:ps)

countQuarts :: [Float] -> (Int, Int, Int, Int)
countQuarts = foldl count (0, 0, 0, 0)
              where count (q1, q2, q3, q4) x | x < 0.25  = (q1+1, q2, q3, q4)
                    count (q1, q2, q3, q4) x | x < 0.5   = (q1, q2+1, q3, q4)
                    count (q1, q2, q3, q4) x | x < 0.75  = (q1, q2, q3+1, q4)
                    count (q1, q2, q3, q4) _ = (q1, q2, q3, q4+1)

checkProbs :: IO (Int, Int, Int, Int)
checkProbs = do
             xs <- probs 10000
             return (countQuarts xs)

{-
Exercise 2 (30 mins)
-}
data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | a + b <= c ||
      a + c <= b ||
      b + c <= a                 = NoTriangle
    | a == b && b == c           = Equilateral
    | a^2 + b^2 == c^2 ||
      a^2 + c^2 == b^2 ||
      b^2 + c^2 == a^2           = Rectangular
    | a == b || b == c || a == c = Isosceles
    | otherwise                  = Other

testTriangle = and [triangle a b c == triangle b c a && triangle a b c == triangle c b a | [a,b,c] <- [[-1..10], [-1..10], [-1..10]]]
testExamples = triangle 1 1 2 == NoTriangle &&
               triangle 3 4 5 == Rectangular &&
               triangle 1 1 1 == Equilateral &&
               triangle 1 2 2 == Isosceles

{-
Exercise 3 (2 hours)
-}
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

{-
Part a
-}
prop1, prop2, prop3, prop4 :: Int -> Bool
prop1 = \x -> even x
prop2 = \x -> even x && x > 3
prop3 = \x -> even x || x > 3
prop4 = \x -> even x && x > 3 || even x

{-
Part b
-}
propList :: [Int -> Bool]
propList = [prop3, prop1, prop2, prop4]

{-
Exercise 4 (30 mins)
-}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation ys [] = False
isPermutation [] ys = False
isPermutation (x:xs) ys = elem x ys && isPermutation xs (delete x ys)

{-
Exercise 5 (2 hours)
-}
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && and (zipWith (/=) xs ys)

-- Create a list with derangement lists
deranList :: Eq a => [a] -> [[a]]
deranList xs = filter (\ys -> isDerangement xs ys) (permutations xs)

-- Properties
-- A list can't be its own derangement
prop51 :: Ord a => [a] -> [a] -> Bool
prop51 [] [] = True
prop51 xs ys = isDerangement xs ys --> not(xs == ys)

-- Derangement is symmetric, if A is a derangement of B then B is a derangement of A
prop52 :: Ord a => [a] -> [a] -> Bool
prop52 xs ys = isDerangement xs ys --> isDerangement ys xs

-- Derangement preservers the length
prop53 :: Ord a => [a] -> [a] -> Bool
prop53 xs ys = isDerangement xs ys --> length xs == length ys

-- Derangement is a permutation
prop54 :: Ord a => [a] -> [a] -> Bool
prop54 xs ys = isDerangement xs ys --> isPermutation xs ys

-- Combine all props for a simple quickCheck
checkProps5 :: Ord a => [a] -> [a] -> Bool
checkProps5 xs ys = and [prop51 xs ys, prop52 xs ys, prop53 xs ys, prop54 xs ys]

-- Result 5:
-- main = quickCheck checkProps5
