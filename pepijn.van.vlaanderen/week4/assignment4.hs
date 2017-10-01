module Lab4 where

import Data.List
import Data.Tuple
import System.Random
import Test.QuickCheck
import SetOrd

{-
Exercise 2 (~1 hour)
-}
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do
    x <-  getRandomInt k
    xs <- getIntL k (n-1)
    return (x:xs)


genIntList :: IO [Int]
genIntList = sequence (replicate 10 (getRandomInt 100))

genIntSet :: IO (Set Int)
genIntSet = do
    xs <- genIntList
    return (list2set xs)

{-
Exercise 3
Make use of intersect and difference operator from List module.
-}
-- unionSet already exists in SetOrd, alternate implementation:
intersectSet :: Ord a => Set a -> Set a -> Set a
intersectSet (Set xs) (Set ys) = list2set (intersect xs ys)

difSet :: Ord a => Set a -> Set a -> Set a
difSet (Set xs) (Set ys) = list2set (xs \\ ys)

-- Check properties
-- Union(A, B) = Union(B, A)
unionProp1 :: Set Int -> Set Int -> Bool
unionProp1 a b = unionSet a b == unionSet b a

-- Every element in A should be in Union(A,B)
unionProp2 :: Set Int -> Set Int -> Bool
unionProp2 (Set a) b = all (\x -> inSet x (unionSet (Set a) b)) a

-- Every element in B should also be an element of Union(A,B)
unionProp3 :: Set Int -> Set Int -> Bool
unionProp3 a (Set b) = all (\x -> inSet x (unionSet a (Set b))) b

-- commutative Intersection Intersection(A, B) = Intersection(B, A)
intersectProp1 :: Set Int -> Set Int -> Bool
intersectProp1 a b = intersectSet a b == intersectSet b a

-- Every element of Intersection(A,B) should be an element of A and B
intersectProp2 :: Set Int -> Set Int -> Bool
intersectProp2 a b = all (\x -> inSet x a && inSet x b) intersect where
  Set (intersect) = intersectSet a b

-- Every element in Difference(A,B) should be an element of A and not an element of B
differenceProp1 :: Set Int -> Set Int -> Bool
differenceProp1 a b = all (\x -> inSet x a && not (inSet x b)) diff where
  Set (diff) = difSet a b

testProp :: (Set Int -> Set Int -> Bool) -> Int -> Int -> IO ()
testProp p n k =
  if n == k then print (show n ++ " tests passed")
  else do x <- genIntSet
          y <- genIntSet
          if not (p x y) then print ("failed test on: " ++ show x)
          else do print ("pass on:" ++ show x)
                  testProp p (n+1) k

-- Alternately test with my own test generator and with quickCheck
main2 = do
        testProp unionProp1 0 10
        testProp unionProp2 0 10
        testProp unionProp3 0 10
        testProp intersectProp1 0 10
        testProp intersectProp2 0 10
        testProp differenceProp1 0 10

{-
Exercise 5
-}
type Rel a = [(a,a)]

-- use swap from data.Tuple
symClos :: Ord a => Rel a -> Rel a
symClos x = sort $ x ++ map swap x

main3 = symClos [(1,2),(2,3),(3,4)]

{-
Exercise 6
-}
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

--trClos :: Ord a => Rel a -> Rel a
