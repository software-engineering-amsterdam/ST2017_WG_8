module Assignment3 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

--Assignment 3: Time: 5 hours
intersectionSet :: Ord a => Set a -> Set a -> Set a
intersectionSet (Set []) _ = Set []
intersectionSet _ (Set []) = Set []
intersectionSet xs (Set (y:ys)) = if inSet y xs then insertSet y (intersectionSet xs (Set ys))
                                                else intersectionSet xs (Set ys)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set []) ys = ys
setUnion xs (Set []) = xs
setUnion xs (Set (y:ys)) = insertSet y (setUnion xs (Set ys))


differenceSet :: Ord a => Set a -> Set a -> Set a
differenceSet (Set []) _ = Set []
differenceSet xs (Set []) = xs
differenceSet (Set (x:xs)) ys = if inSet x ys then differenceSet (Set xs) ys
                                              else insertSet x (differenceSet (Set xs)  ys)

