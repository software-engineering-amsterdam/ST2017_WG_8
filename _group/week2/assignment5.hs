module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

{-
Exercise 5 ~ 2 hours
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
