import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = elem a (permutations b)

prop1 :: [Int] -> [Int] -> Bool --If the first list is a permutation of the second --> The second is a permutation of the first
prop1 xs ys = isPermutation xs ys --> isPermutation ys xs

prop2 :: [Int] -> [Int] -> Bool --If the first list is not a permutation of the second --> The second is not a permutation of the first
prop2 xs ys = not (isPermutation xs ys) --> not (isPermutation ys xs)

prop3 :: [Int] -> [Int] -> Bool 
prop3 xs ys = True



main = print (isPermutation [1,2,3] [3,2,1])

-- time: 5 minutes for now