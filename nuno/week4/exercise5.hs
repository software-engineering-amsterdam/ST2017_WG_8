module Lab4 where
 
import Data.List
import System.Random
import Test.QuickCheck    
import SetOrd

type Rel a = [(a,a)]

insertList x [] = [x]
insertList x ys@(y:ys') = case compare x y of 
                                 GT -> y : insertList x ys' 
                                 EQ -> ys 
                                 _  -> x : ys 

-- Just add the pair (y,x) for each (x,y) if its not in the pairs list already
symClos :: Ord a => Rel a -> Rel a
symClos pairs = symClos' pairs pairs where
    symClos' [] pairs = pairs
    symClos' (x:xs) pairs =  let y = ((snd x), (fst x)) in if elem y pairs then symClos' xs pairs else insertList y (symClos' xs pairs)

main = print (symClos [(1,2), (2,3), (3,4)])

-- time: 10 minutes