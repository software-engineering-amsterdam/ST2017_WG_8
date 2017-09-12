module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck


data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

evaluateTriangle :: Int -> Int -> Int -> Shape
evaluateTriangle x y z | x+y <= z || x+z <= y || y+z <= x = NoTriangle --Triangle inequality (considering the sum of two sides can't be equal to the third)
                       | x == y && y == z = Equilateral
                       | x^2+y^2 == z^2 || x^2+z^2 == y^2 || y^2+z^2 == x^2 = Rectangular
                       | x == y || y == z || x == z = Isosceles
                       | otherwise = Other

-- Test if for all sides (x,y,z), the output of the function is the same for (x,y,z) and (z,y,x)
prop1 = all (\x -> (evaluateTriangle (head x) (head (tail x)) (head (tail (tail x)))) == (evaluateTriangle (head (tail (tail x))) (head (tail x)) (head x))) 
            [[x,y,z] | x <- [-1..10], y <- [-1..10], z <- [-1..10]] -- Nevative sides should fail the triangle inequality property

testSamples = (evaluateTriangle 5 3 4) == Rectangular && 
              (evaluateTriangle 1 1 1) == Equilateral && 
              (evaluateTriangle 0 10 1) == NoTriangle && 
              (evaluateTriangle 10 10 5) == Isosceles

main = print prop1
-- time: 20 minutes