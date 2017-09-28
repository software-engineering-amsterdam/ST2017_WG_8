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

main = print (trClos [(1,2), (2,3), (3,4)])

-- time: 5 minutes