--module Lab4 where
 
import Data.List
import System.Random
import Test.QuickCheck    
import SetOrd

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

insertList x [] = [x]
insertList x ys@(y:ys') = case compare x y of 
                                 GT -> y : insertList x ys' 
                                 EQ -> ys 
                                 _  -> x : ys 


-- Union of R^k until we reach a certain k for which the union with R^(k+1) doesn't add more elements (meaning its transitive)
trClos :: Ord a => Rel a -> Rel a
trClos xs = let compXs = sort (nub ((xs @@ xs)++xs)) in if xs == compXs then xs else trClos compXs

-- Just add the pair (y,x) for each (x,y) if its not in the pairs list already
symClos :: Ord a => Rel a -> Rel a
symClos pairs = symClos' pairs pairs where
    symClos' [] pairs = pairs
    symClos' (x:xs) pairs =  let y = ((snd x), (fst x)) in if elem y pairs then symClos' xs pairs else insertList y (symClos' xs pairs)

symTransProp :: Rel Int -> Bool
symTransProp r = trClos (symClos r) == symClos (trClos r)

main = quickCheck symTransProp
-- As we can see, there IS a difference between the symmetric closure of the transitive closure of a relation R and 
--                                              the transitive closure of the symmetric closure of that relation
-- quickCheck gave us a short counter-example --> [(0,1)]
-- symClos [(0,1)] = [(0,1),(1,0)]
-- trClos [(0,1)] = [(0,1)]
-- therefore. trClos (symClos [(0,1)]) = [(0,0),(0,1),(1,0),(1,1)]
--     and    symClos (trClos [(0,1)]) = [(0,1), (1,0)]

-- time: 5 minutes