module Lab4 where
 
import Data.List
import System.Random
import Test.QuickCheck    
import SetOrd

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Union of R^k until we reach a certain k for which the union with R^(k+1) doesn't add more elements (meaning its transitive)
trClos :: Ord a => Rel a -> Rel a
trClos xs = let compXs = sort (nub ((xs @@ xs)++xs)) in if xs == compXs then xs else trClos compXs

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

transitive :: Eq a => Rel a -> Bool
transitive r = and [elem (x,z) r | (x,y) <- r, (w,z) <- r, y == w]

symmetric :: Eq a => Rel a -> Bool
symmetric r = and [elem (y,x) r | (x,y) <- r]

-- The transitive closure of a relation is transitive
propTrans1 :: Rel Int -> Bool
propTrans1 xs = let rel = sort (nub xs) in transitive (trClos rel)

-- The original relation is contained in its transitive closure
propTrans2 :: Rel Int -> Bool
propTrans2 xs = let closure = trClos xs in all (\x -> elem x closure)  xs

-- The symmetric closure of a relation is symmetric
propSym1 :: Rel Int -> Bool
propSym1 xs = let rel = sort (nub xs) in symmetric (symClos rel)

-- The original relation is contained in its transitive closure
propSym2 :: Rel Int -> Bool
propSym2 xs = let closure = symClos xs in all (\x -> elem x closure)  xs

-- We can use quickCheck the same way as we use other types since Rel is just another name for [(a,a)]. 
-- The only problem is [(a,a)] isn't necessarily an ordered list, so we should take measures to prevent that from being a problem
main = quickCheck propSym2


-- time: 20 minutes for now

-- TODO: I dont see a way to test whether the result of symClos or trClos is the smallest symmetric/transitive set or not