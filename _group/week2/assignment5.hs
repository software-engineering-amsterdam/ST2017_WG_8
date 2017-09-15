module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- 4. Time spent: ~ 1 hour

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = False
isDerangement x y = (isPermutation x y) && parsePermutation x y

parsePermutation :: Eq a => [a] -> [a] -> Bool
parsePermutation [] [] = True
parsePermutation x y = if (head x == head y ) then False else parsePermutation (tail(x)) (tail(y)) 


deran :: Eq a => [a] -> [[a]]
deran [] = []
deran x = filter (\y -> isDerangement x y) (permutations x)

-- A list can't be its own derangement
property51 :: [Int] -> [Int] -> Bool
property51 x y = isDerangement x y --> not(x == y)

-- Derangement is symmetric, if A is a derangement of B then B is a derangement of A
property52 :: [Int] -> [Int] -> Bool
property52 x y = isDerangement x y --> isDerangement y x


testR2 :: Int -> Int -> ([Int] -> [Int] -> Bool) -> IO ()
testR2 k n f = if k == n then print (show n ++ " tests passed")
                else do
                  x <- genIntList 20 10
                  y <- genIntList 20 10
                  if (f x y) then
                    testR2 (k+1) n f
                  else error ("failed: " ++ show x ++ show y)

checkBatch5 :: IO()
checkBatch5 = do
    print("Testing property 1")
    testR2 0 100 property51
    print("Testing property 2")
    testR2 0 100 property52




-- Result 5:
-- main = print(isDerangement <list> <list>)
-- main = print(deran <list>)
-- main = checkBatch5
