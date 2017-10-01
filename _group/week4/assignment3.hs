{-# LANGUAGE FlexibleInstances #-}
module Lab4 where
 
import Data.List
import System.Random
import Test.QuickCheck    
import SetOrd


-- Testing functions ---------------------------

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   xs <- getIntL k (n-1)
   return (x:xs)

genIntList :: Int -> Int -> IO [Int]
genIntList kRange nRange = do 
  k <- getRandomInt kRange
  n <- getRandomInt nRange
  getIntL k n


genSets :: IO (Set Int)
genSets = do x <- genIntList 99 50
             return (list2set x)


instance Arbitrary (Set Int) where
    arbitrary = list2set <$> lst where 
        lst = getOrdered <$> arbitrary

-- A few twists so the function takes 2 sets instead of one.
testProps :: Int -> Int -> (Set Int -> Set Int -> Bool) -> IO ()
testProps k n f = if k == n then print (show n ++ " tests passed")
                else do
                  s <- genSets
                  s2 <- genSets
                  if (f s s2) then
                    testProps (k+1) n f
                  else error ("failed: " ++ show s ++ "|" ++ show s2)

--------------------------------------------------


-- Union is already implemented in SetOrd

-- Intersection of set A and set B = A ^ B
intersection :: Ord a => Set a -> Set a -> Set a 
intersection (Set []) _ = emptySet
intersection (Set (s:s1)) s2 = if inSet s s2 then insertSet s (intersection (Set s1) s2) else intersection (Set s1) s2

difference :: Ord a => Set a -> Set a -> Set a
difference (Set []) _ = emptySet
difference (Set (s:s1)) s2 = if not (inSet s s2) then insertSet s (difference (Set s1) s2) else difference (Set s1) s2

-- commutative Union(A, B) = Union(B, A)
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
intersectProp1 a b = intersection a b == intersection b a

-- Every element of Intersection(A,B) should be an element of A and B
intersectProp2 :: Set Int -> Set Int -> Bool
intersectProp2 a b = all (\x -> inSet x a && inSet x b) intersect where
  Set (intersect) = intersection a b

-- Every element in Difference(A,B) should be an element of A and not an element of B
differenceProp1 :: Set Int -> Set Int -> Bool
differenceProp1 a b = all (\x -> inSet x a && not (inSet x b)) diff where
  Set (diff) = difference a b

-- Alternately test with my own test generator and with quickCheck
main = do print "Testing union commutativity"
          testProps 0 100 unionProp1
          quickCheck unionProp1
          print "Testing union prop2"
          testProps 0 100 unionProp2
          quickCheck unionProp2
          print "Testing union prop3"
          testProps 0 100 unionProp3
          quickCheck unionProp3
          print "Testing intersection commutativity"
          testProps 0 100 intersectProp1
          quickCheck intersectProp1
          print "Testing if all elements of an intersection also belong to both of the original sets"
          testProps 0 100 intersectProp2
          quickCheck intersectProp2
          print "Testing if all elements of a difference between sets A and B are elements of A and not of B"
          testProps 0 100 differenceProp1
          quickCheck differenceProp1


-- time: 55 minutes