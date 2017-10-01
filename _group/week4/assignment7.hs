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

-- Testing functions ---------------------------

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getPairsL :: Int -> Int -> IO (Rel Int)
getPairsL _ 0 = return []
getPairsL k n = do 
   x <-  getRandomInt k
   y <-  getRandomInt k
   xs <- getPairsL k (n-1)
   return ((x,y):xs)

getPairsList :: Int -> Int -> IO (Rel Int)
getPairsList kRange nRange = do 
  k <- getRandomInt kRange
  n <- getRandomInt nRange
  getPairsL k n


genRels :: IO (Rel Int)
genRels = getPairsList 100 20

-- A few twists so the function takes 2 sets instead of one.
testProps :: Int -> Int -> (Rel Int -> Bool) -> IO ()
testProps k n f = if k == n then print (show n ++ " tests passed")
                else do
                  s <- genRels
                  if (f s) then
                    testProps (k+1) n f
                  else error ("failed: " ++ show s)

--------------------------------------------------

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

-- The transitive closure of a relation is as small as possible
--  We simply try to remove the added elements from the transitive closure and check if its still transitive
propTrans3 :: Rel Int -> Bool
propTrans3 xs = all (\x -> not (transitive (delete x closure))) (filter (\x -> not (elem x xs)) closure) where
    closure = (trClos xs)

-- The symmetric closure of a relation is symmetric
propSym1 :: Rel Int -> Bool
propSym1 xs = let rel = sort (nub xs) in symmetric (symClos rel)

-- The original relation is contained in its transitive closure
propSym2 :: Rel Int -> Bool
propSym2 xs = let closure = symClos xs in all (\x -> elem x closure)  xs

-- The symmetric closure of a relation is as small as possible
-- To check that we basically see if its true that for every element (x,y) such that (x,y) is not an element of A but still 
--    an element of the closure of A, if (y,x) is an element of A. (If it isn't, then its a superfluous pair)
propSym3 :: Rel Int -> Bool
propSym3 xs = all (\(x,y) -> elem (y,x) xs) (filter (\x -> not (elem x xs)) (symClos xs))

-- We can use quickCheck the same way as we use other types since Rel is just another name for [(a,a)]. 
-- The only problem is [(a,a)] isn't necessarily an ordered list, so we should take measures to prevent that from being a problem
main = do print "Testing properties with quickcheck"
          quickCheck propTrans1
          quickCheck propTrans2
          quickCheck propTrans3
          quickCheck propSym1
          quickCheck propSym2
          quickCheck propSym3
          print "Testing properties with my own test generator"
          testProps 0 100 propTrans1
          testProps 0 100 propTrans2
          testProps 0 100 propTrans3
          testProps 0 100 propSym1
          testProps 0 100 propSym2
          testProps 0 100 propSym3

-- time: 1 hour