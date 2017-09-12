module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

--Assignment 1: Time: 2,5 hours
quartiles :: [Float] -> [Int]
quartiles xs = [f1 xs, f2 xs, f3 xs, f4 xs] 
    where
        f1 x = length(filter (\x -> x <= 0.25) x)
        f2 x = length(filter (\x -> x > 0.25 && x <= 0.50) x)
        f3 x = length(filter (\x -> x > 0.50 && x <= 0.75) x)
        f4 x = length(filter (\x -> x > 0.75 && x <= 1.00) x)

testQuartiles = do
    n <- probs 10000
    putStrLn (show (quartiles n))


--Assignment 2: Time: 2,5 hours
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | isNoTriangle a b c = NoTriangle 
               | isTriangle a b c && isEquilateral a b c = Equilateral
               | isTriangle a b c && isIsosceles a b c = Isosceles
               | isTriangle a b c && isRectangular a b c = Rectangular
               | otherwise = Other


--
isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle a b c = a+b>c && a+c>b && b+c>a

--
isNoTriangle :: Integer -> Integer -> Integer -> Bool
isNoTriangle a b c = not $ a+b>c && a+c>b && b+c>a

-- An equilateral triangle has all sides the same length
isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c = a==b && b==c

-- An isosceles triangle has two sides of equal length
isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c = a==b || a==c || b==c

--
isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = a^2+b^2==c^2 || a^2+c^2==b^2 || b^2+c^2==a^2


--Assignment 3: Time: 2 hours
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

--(\ x -> even x && x > 3) even
--(\ x -> even x || x > 3) even
--(\ x -> (even x && x > 3) || even x) even
--even (\ x -> (even x && x > 3) || even x)
firstProp :: Int -> Bool
firstProp x = even x && x > 3

secondProp :: Int -> Bool
secondProp x = even x || x > 3

thirdProp :: Int -> Bool
thirdProp x = (even x && x > 3) || even x

fourthProp :: Int -> Bool
fourthProp x = mod x 2 == 0


--Assignment 4: Time: 1 hours
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = xs `elem` (permutations ys)

testPermutations = do
    putStr "- isPermutation [5, 7, 2] [1, 2, 3] : "
    putStrLn (show(isPermutation [5, 7, 2] [1, 2, 3]))
    putStr "- isPermutation [5, 8, 4] [4, 5, 8] : "
    putStrLn (show(isPermutation [5, 8, 4] [4, 5, 8]))
    putStr "- isPermutation [2, 4, 4] [ 2, 4, 6] : "
    putStrLn (show(isPermutation [2, 4, 4] [2, 4, 6]))
    putStr "- isPermutation [1, 3, 2] [1, 2, 3] : "
    putStrLn (show(isPermutation [1, 3, 2] [1, 2, 3]))
    putStr "- isPermutation [11, 33, 22, 22] [11, 22, 33] : "
    putStrLn (show(isPermutation [11, 33, 22, 22] [11, 22, 33]))