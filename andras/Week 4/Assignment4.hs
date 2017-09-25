import Data.List
import System.Random
import Test.QuickCheck
import SetOrd


-- Limit quickCheck to a list of numbers between 0 and 4 ()
genLimit :: Gen Int
genLimit = choose (-1000, 1000)

genListOfLimit :: Gen [Int]
genListOfLimit = listOf genLimit

-- To calculate the lenght we use the fact that a set will only be a subset of its first n elements if n == length(set) 
setLength :: Ord a => Set a -> Int
setLength s = (setLengthN s 0) 

setLengthN :: Ord a => Set a -> Int -> Int
setLengthN s n = if subSet s (takeSet n s) then n else setLengthN s (n+1) 
--setLengthN s 10000 = -1 -- Set a limit just in case of an infinite recursion


------------------------------------------------------------------
-- 1)



------------------------------------------------------------------
-- 2)

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


------------------------------------------------------------------
-- 3)

{-
unionSet :: (Ord a) => Set a -> Set a -> Set a 
unionSet (Set [])     set2  =  set2
unionSet (Set (x:xs)) set2  = 
   insertSet x (unionSet (Set xs) set2)
-}

intersectSet :: (Ord a) => Set a -> Set a -> Set a 
intersectSet (Set [])     set2  =  (Set [])


differenceSet :: (Ord a) => Set a -> Set a -> Set a 
differenceSet (Set [])     set2  =  set2


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




main = runDataGen2

