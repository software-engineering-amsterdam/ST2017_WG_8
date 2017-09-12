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
-- main = checkBatch3

{- 
Order from strongest to weakest
Strong
---------------------
(\ x -> even x && x > 3)
(\ x -> (even x && x > 3) || even x) 
even
(\ x -> even x || x > 3)
---------------------
Weak
-}