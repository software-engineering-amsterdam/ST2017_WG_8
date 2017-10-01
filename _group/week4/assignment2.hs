{-# LANGUAGE FlexibleInstances #-}

module Lab4 where
 
import Data.List
import System.Random
import Test.QuickCheck    
import SetOrd

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x1] = True
sorted (x1:x2:xs) = x1 <= x2 && sorted (x2:xs)


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


genSets :: IO (Set Int)
genSets = do x <- genIntList 99 50
             return (list2set x)


instance Arbitrary (Set Int) where
    arbitrary = list2set <$> lst where 
        lst = getOrdered <$> arbitrary

-- A set is always a subset of itself
prop1 :: Set Int -> Bool
prop1 s = (subSet s s)

-- Empty set is a subset of every set
prop2 :: Set Int -> Bool
prop2 s = subSet emptySet s

-- A set doesn't contain duplicate elements
prop3 :: Set Int -> Bool
prop3 (Set s) = s == nub s

-- A set is an ordered list
prop4 :: Set Int -> Bool
prop4 (Set s) = sorted s

testProps :: Int -> Int -> (Set Int -> Bool) -> IO ()
testProps k n f = if k == n then print (show n ++ " tests passed")
                else do
                  s <- genSets
                  if (f s) then
                    testProps (k+1) n f
                  else error ("failed: " ++ show s)

main = do quickCheck prop4
          testProps 0 100 prop3

-- time: 2h30m