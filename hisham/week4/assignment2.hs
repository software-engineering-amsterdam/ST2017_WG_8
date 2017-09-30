module Assignment2 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

--Assignment 2: Time: 4 hours

 -- Returns a random IO Int from a given Int tuple 
nRandom :: (Int, Int) -> IO Int
nRandom (l, h) = getStdRandom (randomR (l, h))


-- Returns IO Set of random numbers from a list
genSet :: IO (Set Int)
genSet = fmap list2set ( nRandom (0, 100) >>= genIList)

genIList :: (Num t, Eq t) => t -> IO [Int]
genIList 0 = return []
genIList len = do
     x <- nRandom (-50, 50)
     xs <- genIList (len-1)
     return (x:xs)
