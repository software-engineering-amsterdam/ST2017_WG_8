import Data.List
import System.Random
import Test.QuickCheck
import SetOrd


-- Limit quickCheck to a list of numbers between 0 and 4 ()
genLimit :: Gen Int
genLimit = choose (-1000, 1000)

genListOfLimit :: Gen [Int]
genListOfLimit = listOf genLimit




------------------------------------------------------------------
-- 1)



------------------------------------------------------------------
-- 2) Time spent ~ 45 minutes

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


-- Create an [Int], this will be the seed for the formula
createSeed ::  IO [Int]
createSeed = do
    x <- getRandomInt 20
    y <- getRandomInt 100
    xs <- getIntL (y) (x)
    return xs

seedToSet :: [Int] -> Set Int
seedToSet x = list2set x

runDataGen1 :: IO (Set Int)
runDataGen1 = do
    xs <- (createSeed)
    let s = seedToSet xs 
    print(s)
    print(setLength s)
    return(s)


-- A set can't contain duplicate elements
setProperty1 :: Ord a => Set a -> Bool
setProperty1 (Set []) = True 
setProperty1 (Set (x:xs)) = if inSet x (Set(xs)) then False else setProperty1 (Set(xs))

-- A set is always a subset of itself, and an empty set is an element of every set
setProperty2 :: Ord a =>  Set a -> Bool
setProperty2 s = (subSet s s) && (subSet (Set []) s) 

-- TODO
runDataGen2 :: IO()
runDataGen2 = do
    quickCheck (\x -> setProperty1 (seedToSet x))
    quickCheck (\x -> setProperty2 (seedToSet x))

-- Result 2:
-- main = runDataGen1
-- main = runDataGen2

------------------------------------------------------------------
-- 3)


-- To calculate the lenght we use the fact that a set will only be a subset of its first n elements if n == length(set) 
setLength :: Ord a => Set a -> Int
setLength s = (setLengthN s 0) 

setLengthN :: Ord a => Set a -> Int -> Int
setLengthN s n = if subSet s (takeSet n s) then n else setLengthN s (n+1) 
--setLengthN s 10000 = -1 -- Set a limit just in case of an infinite recursion


intersectSet :: (Ord a) => Set a -> Set a -> Set a 
intersectSet (Set [])     set2  =  (Set [])
intersectSet set1 set2  =  intersectSetN set1 set2 (Set []) (setLength set1)

-- We check each element of set1 if they are an element of set2, if yes, we add them to set3. When we parsed through all the elements of set1 we return set3 
intersectSetN :: (Ord a) => Set a -> Set a -> Set a -> Int -> Set a
intersectSetN set1 set2 set3 0 = set3
intersectSetN (Set (x:xs)) set2 set3 n = if inSet x set2 then intersectSetN (Set (xs)) set2 (insertSet x set3) (n-1) else intersectSetN (Set (xs)) set2 set3 (n-1) 


-- A difference set of set1 and set2 is the same as (set1 ∨ set2) - (set1 ∧ set2)
differenceSet :: (Ord a) => Set a -> Set a -> Set a 
differenceSet (Set [])     set2  =  set2
differenceSet set1 set2 = differenceSetN (unionSet set1 set2) (intersectSet set1 set2) (Set []) (setLength(unionSet set1 set2))

-- We check each element of set1 if they are an element of set2, if no, we add them to set3. When we parsed through all the elements of set1 we return set3
differenceSetN ::  (Ord a) => Set a -> Set a -> Set a -> Int -> Set a
differenceSetN set1 set2 set3 0 = set3
differenceSetN (Set (x:xs)) set2 set3 n = if not(inSet x set2) then differenceSetN (Set (xs)) set2 (insertSet x set3) (n-1) else differenceSetN (Set (xs)) set2 set3 (n-1) 


--setOperationProperty1 :: Ord a =>  Set a -> Set a -> Bool
--setOperationProperty1

------------------------------------------------------------------
-- 4)


------------------------------------------------------------------
-- 5)

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos r = r

------------------------------------------------------------------
-- 6)

infixr 5 @@ 

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

------------------------------------------------------------------
-- 7)


------------------------------------------------------------------
-- 8)




main = print(differenceSet (Set [1, 2, 3, 4]) (Set [1, 2, 5, 6]))
