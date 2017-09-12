module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

{-
Exercise 2 (30 mins)
-}
data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | a + b <= c ||
      a + c <= b ||
      b + c <= a                 = NoTriangle
    | a == b && b == c           = Equilateral
    | a^2 + b^2 == c^2 ||
      a^2 + c^2 == b^2 ||
      b^2 + c^2 == a^2           = Rectangular
    | a == b || b == c || a == c = Isosceles
    | otherwise                  = Other

testTriangle = and [triangle a b c == triangle b c a && triangle a b c == triangle c b a | [a,b,c] <- [[-1..10], [-1..10], [-1..10]]]
testExamples = triangle 1 1 2 == NoTriangle &&
               triangle 3 4 5 == Rectangular &&
               triangle 1 1 1 == Equilateral &&
               triangle 1 2 2 == Isosceles

-- Test if for all sides (x,y,z), the output of the function is the same for (x,y,z) and (z,y,x)
prop1 = all (\x -> (triangle (head x) (head (tail x)) (head (tail (tail x)))) == (triangle (head (tail (tail x))) (head (tail x)) (head x)))
           [[x,y,z] | x <- [-1..10], y <- [-1..10], z <- [-1..10]] -- Nevative sides should fail the triangle inequality property