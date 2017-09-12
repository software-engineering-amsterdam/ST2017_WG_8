import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = elem a (permutations b)

isDerangementAux :: Eq a => [a] -> [a] -> Bool
isDerangementAux [] [] = True
isDerangementAux (x:xs) (y:ys) = x /= y && isDerangementAux xs ys

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && isDerangementAux xs ys

--main = print (isDerangement [1,2,3] [3,2,1]) --false
--main = print (isDerangement [1,2,3] [2,3,1]) --true

-- time: 5 minutes for now