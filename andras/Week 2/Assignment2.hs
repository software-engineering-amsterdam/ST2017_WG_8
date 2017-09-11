
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 
 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-----------------------------------------------------------------------------------------
-- 1. Time spent: ~ 25 minutes (of which 15 minutes was fighting with IO [Float ])

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

checkRandom :: [Float] -> [Int]
checkRandom [] = [0, 0, 0, 0]
checkRandom x = [(length(filter(<=0.25) x)), (length(filter(\x -> x > 0.25 && x <= 0.5) x)), (length(filter(\x -> x > 0.5 && x <= 0.75) x)), (length(filter(\x -> x > 0.75 && x <= 1) x))]


{-

Result 1:
main :: IO()
main = do
			x <- probs 10000
			-- We assume that if a container has more/less than 2500 +- (2500 * 0.05) the distribution is not random enough
			print(if length(filter (\x -> x >= 2375 && x <= 2625) (checkRandom x)) == 4 then "Random distribution checks out" else "Distribution is not random")
-}


-----------------------------------------------------------------------------------------
-- 2. Time spent: ~ 15 minutes +  ~ 15 minutes for tests + batch 

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)



triangle :: Integer -> Integer -> Integer -> Shape 
triangle x y z | (x >= y + z || y > x + z || z > x + y || x <= 0 || y <= 0 || z <= 0) = NoTriangle
               | (x == y && y == z ) = Equilateral
               | (x == y || x == z || y == z) = Isosceles
               | (x^2 == y^2 + z^2 || y^2 == x^2 + z^2 || z^2 == x^2 + y^2) = Rectangular
               | otherwise = Other

-- These tests are a bit trivial since we're checking for the same conditions that are in the actual function, failing the test is virtually impossible, but technically they should be correct
-- A small difference is that we didn't need to specif that x, y, and z are > 0, since the first pattern matching statement should catch any other values, in the quickCheck we have to specify this each time
checkNo = quickCheck (\x y z -> (x >= y + z || y > x + z || z > x + y || x <= 0 || y <= 0 || z <= 0) --> triangle x y z == NoTriangle)
checkEq = quickCheck (\x y z -> (x == y && y == z && x > 0 && y > 0 && z > 0)  --> triangle x y z == Equilateral)
checkIso = quickCheck (\x y z -> (x == y || x == z || y == z) &&( x > 0 && y > 0 && z > 0) --> triangle x y z == Isosceles)
checkRec = quickCheck (\x y z -> (x^2 == y^2 + z^2 || y^2 == x^2 + z^2 || z^2 == x^2 + y^2) && ( x > 0 && y > 0 && z > 0) --> triangle x y z ==  Rectangular)
checkOther = quickCheck (\x y z -> (x >= y + z || y > x + z || z > x + y) && (x > 0 && y > 0 && z > 0) && not(x == y || x == z || y == z) && not(x^2 == y^2 + z^2 || y^2 == x^2 + z^2 || z^2 == x^2 + y^2) --> triangle x y z == NoTriangle)

checkBatch2 :: IO()
checkBatch2 = do
    print("Not-a-triangle check:")
    checkNo
    print("Equilateral check:")
    checkEq
    print("Isosceles check:")
    checkIso
    print("Rectangular check:")
    checkRec
    print("Other check:")
    checkOther

-- Result 2:
-- main = print(trinagle <Int1> <Int2> <Int3>)
-- main = checkBatch2

-----------------------------------------------------------------------------------------
-- 3. Time spent: ~ 20 minutes

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

x1s = stronger [-10..10](\ x -> even x && x > 3) even -- True
x1w = weaker [-10..10](\ x -> even x && x > 3) even -- False
-- (\ x -> even x && x > 3) is stronger than 'even'

x2s = stronger [-10..10](\ x -> even x || x > 3) even -- False
x2w = weaker [-10..10](\ x -> even x || x > 3) even -- True

x3s = stronger [-10..10](\ x -> (even x && x > 3) || even x) even -- True 
x3w = weaker [-10..10](\ x -> (even x && x > 3) || even x) even -- True

x4s = stronger [-10..10] even (\ x -> (even x && x > 3) || even x) -- True
x4w = weaker [-10..10] even (\ x -> (even x && x > 3) || even x) -- True

checkBatch3 :: IO()
checkBatch3 = do
    print("1")
    print("stronger")
    print(x1s)
    print("weaker")
    print(x1w)
    print("----------------------")
    print("2")
    print("stronger")
    print(x2s)
    print("weaker")
    print(x2w)
    print("----------------------")
    print("3")
    print("stronger")
    print(x3s)
    print("weaker")
    print(x3w)
    print("----------------------")
    print("4")
    print("stronger")
    print(x4s)
    print("weaker")
    print(x4w)
    print("----------------------")


-- Result 3:
-- main = checkBatch2

{- 
Order from strongest to weakest
Strong
---------------------
(\ x -> even x && x > 3)
(\ x -> even x || x > 3)
(\ x -> (even x && x > 3) || even x), even
---------------------
Weak
-}


-----------------------------------------------------------------------------------------
-- 4. Time spent: ~ 20 minutes

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [x] [y] = [x] elem (subsequences [y]) 



main = checkBatch3