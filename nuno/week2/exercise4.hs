module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Auxiliary functions for testing purposes
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   xs <- getIntL k (n-1)
   return (x:xs)

genIntList :: Int -> Int -> IO [Int]
genIntList kRange nRange = do 
  k <- getRandomInt kRange
  n <- getRandomInt nRange
  getIntL k n
-- ----------------------------------------

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = elem a (permutations b)

-- Switch the order of the first and second elements of a list
change2First :: [Int] -> [Int]
change2First (x1:x2:xs) = (x2:x1:xs)
change2First xs = xs -- Just in case the list doesn't have 2 or more elements

--If the first list is a permutation of the second --> The second is a permutation of the first
prop1 :: [Int] -> Bool
prop1 xs = isPermutation xs (change2First xs) --> isPermutation (change2First xs) xs

--If the first list is not a permutation of the second --> The second is not a permutation of the first
prop2 :: [Int] -> Bool
prop2 xs = not (isPermutation xs (change2First xs)) --> not (isPermutation (change2First xs) xs)

-- A list is always a permutation of itself
prop3 :: [Int] -> Bool 
prop3 xs = isPermutation xs xs

-- Testing a property without quickCheck
testProps :: Int -> Int -> ([Int] -> Bool) -> IO ()
testProps k n f = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList 20 10
                  if (f xs) then
                    testProps (k+1) n f
                  else error ("failed: " ++ show xs)

-- main = quickCheck prop1       -- Testing a property with quickCheck
main = testProps 0 100 prop1     -- Testing a property without quickCheck

-- You may assume that your input lists do not contain duplicates. What does this mean for your testing procedure?
--          It means I dont have to test situations where I have duplicates in a list.

-- Ordered list of properties by strength (descendent): prop1, prop2, prop3

-- time: 40 minutes