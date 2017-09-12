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

allDifferent :: (Eq a) => [a] -> Bool
allDifferent [] = True
allDifferent (x:xs) = notElem x xs && allDifferent xs

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = elem a (permutations b)

isDerangementAux :: Eq a => [a] -> [a] -> Bool
isDerangementAux [] [] = False  -- It was discussed in class that if lists are equal than one is not a derangement of the other, including for empty lists.
isDerangementAux [x] [y] = x /= y
isDerangementAux (x:xs) (y:ys) = x /= y && isDerangementAux xs ys

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && isDerangementAux xs ys

deran :: Eq a => [a] -> [[a]]
deran xs = filter (\x -> isDerangementAux x xs) (permutations xs)

-- A list is never a derangement of itself
prop1 :: [Int] -> [Int] -> Bool
prop1 xs ys = isDerangement xs ys --> xs /= ys

-- If a list is a derangement of a second list, the second will be a derangement of the first
prop2 :: [Int] -> [Int] -> Bool
prop2 xs ys = isDerangement xs ys --> isDerangement ys xs

-- If a list is not a derangement of a second list, the second will not be a derangement of the first
prop3 :: [Int] -> [Int] -> Bool
prop3 xs ys = not (isDerangement xs ys) --> not (isDerangement ys xs)

-- Testing a property without quickCheck
testProps :: Int -> Int -> ([Int] -> [Int] -> Bool) -> IO ()
testProps k n f = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- genIntList 20 10
                  ys <- genIntList 20 10
                  if (f xs ys) then
                    testProps (k+1) n f
                  else error ("failed: " ++ show xs ++ show ys)

main = testProps 0 100 prop1     -- Testing a property without quickCheck

--main = print (isDerangement [1,2,3] [3,2,1]) --false
--main = print (isDerangement [1,2,3] [2,3,1]) --true
--main = print (derangements [1,2,3,4])        -- [[4,3,2,1],[3,4,2,1],[2,3,4,1],[4,1,2,3],[2,4,1,3],[2,1,4,3],[4,3,1,2],[3,4,1,2],[3,1,4,2]]

-- Ordered list of properties by their strength (descending): prop2, prop3, prop1

-- time: 45 minutes for now