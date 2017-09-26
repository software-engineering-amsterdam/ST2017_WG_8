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

-- A few canges so the function takes 2 sets instead of one.
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

identityProp :: Set Int -> Bool
identityProp s = unionSet s emptySet == s

dominationProp :: Set Int -> Bool
dominationProp s = intersection s emptySet == emptySet

-- commutative Union(A, B) = Union(B, A)
comutUnionProp :: Set Int -> Set Int -> Bool
comutUnionProp a b = unionSet a b == unionSet b a

-- commutative Intersection Intersection(A, B) = Intersection(B, A)
comutIntersectionProp :: Set Int -> Set Int -> Bool
comutIntersectionProp a b = intersection a b == intersection b a

-- A U (B U C) == (A U B) U C
associativityProp1 :: Set Int -> Set Int -> Set Int -> Bool
associativityProp1 a b c = unionSet a (unionSet b c) == unionSet (unionSet a b) c

-- A ^ (B ^ C) == (A ^ B) ^ C
associativityProp2 :: Set Int -> Set Int -> Set Int -> Bool
associativityProp2 a b c = intersection a (intersection b c) == intersection (intersection a b) c

-- A U (B ^ C) = (A U B) ^ (A U C)
distributivityProp1 :: Set Int -> Set Int -> Set Int -> Bool
distributivityProp1 a b c = unionSet a (intersection b c) == intersection (unionSet a b) (unionSet a c)

-- A ^ (B U C) = (A ^ B) U (A ^ C)
distributivityProp2 :: Set Int -> Set Int -> Set Int -> Bool
distributivityProp2 a b c = intersection a (unionSet b c) == unionSet (intersection a b) (intersection a c)

main = quickCheck distributivityProp2
--main = print (intersection (list2set [1,2,3]) (list2set [2,3,4])) -- Should return {2,3}
--main = print (difference (list2set [1,2,3]) (list2set [2,3,4])) -- Should return {1}

-- time: 40 minutes

-- TODO: my own generator giving different arguments accordingly to the function???
-- Make a main with do all tests