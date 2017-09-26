{-# LANGUAGE FlexibleInstances #-}

--module Lab4 where
 
import Data.List
import System.Random
import Test.QuickCheck    
import SetOrd

instance Arbitrary (Set Int) where
    arbitrary = Set <$> lst where 
        lst = getOrdered <$> arbitrary

-- A set is always a subset of itself
prop1 :: Set Int -> Bool
prop1 s = (subSet s s)

main = quickCheck prop1