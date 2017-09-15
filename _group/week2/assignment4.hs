module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- 4. Time spent: ~ 1 hour

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

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

-- Switch the order of the first and second elements of a list
change2First :: [Int] -> [Int]
change2First (x1:x2:xs) = (x2:x1:xs)
change2First xs = xs

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation x y = elem x (permutations y)


-- A permutation of a list is always the same length as the original list 
property41 :: [Int] -> Bool 
property41 x = length x == length(head(permutations x))

-- Permutation is reflexive, list is always a permutation of itself
property42 :: [Int] -> Bool 
property42 x = isPermutation x x

-- Permutation is symmetric, if A is a permutation of B then B is a permutation of A
property43 :: [Int] -> Bool
property43 x = isPermutation x (change2First x) --> isPermutation (change2First x) x

--If the first list is not a permutation of the second --> The second is not a permutation of the first
property44 :: [Int] -> Bool
property44 x = not (isPermutation x (change2First x)) --> not (isPermutation (change2First x) x)


testR1 :: Int -> Int -> ([Int] -> Bool) -> IO ()
testR1 k n f = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList 20 10
                  if (f xs) then
                    testR1 (k+1) n f
                  else error ("failed: " ++ show xs)

checkBatch4 :: IO()
checkBatch4 = do
    print("Testing property 1")
    testR1 0 100 property41
    print("Testing property 2")
    testR1 0 100 property42
    print("Testing property 3")
    testR1 0 100 property43
    print("Testing property 4")
    testR1 0 100 property44

-- Result 4:
-- main = print(isPermutation [1, 2, 3] [3, 1, 2])
-- main = checkBatch4