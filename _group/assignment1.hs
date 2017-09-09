module Lab1 where
import Data.List
import Test.QuickCheck

-- Assignment 1 #Time: 15 mins

infix 1 --> 
 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q
 
 
-- Workshop exercise 2
ws21, ws22 :: Int -> Int
ws21 = \n -> sum (map (^2) [0..n])
ws22 = \n -> n*(n+1)*(2*n+1) `div` 6

test_ws2 = quickCheckResult (\n -> n >= 0 --> ws21 n == ws22 n)

-- Workshop exercise 3
ws31, ws32 :: Int -> Int
ws31 = \n -> sum (map (^3) [0..n])
ws32 = \n -> (n*(n+1) `div` 2)^2

test_ws3 = quickCheckResult (\n -> n >= 0 --> ws31 n == ws32 n)