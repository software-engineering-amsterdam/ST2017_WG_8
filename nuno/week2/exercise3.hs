module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

forall :: [Int] -> (Int -> Bool) -> Bool
forall = flip all

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)

weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
weaker   xs p q = stronger xs q p 

-- a)
first :: Int -> Bool
first x = even x && x > 3

second :: Int -> Bool
second x = even x || x > 3

third :: Int -> Bool
third x = (even x && x > 3) || even x
-- fourth is the same as third
-- "even" property is already implemented. but its basically (\x -> mod x 2 == 0)

main = print "Descending list of properties accordingly to their strength: [first, third, even, second]"
-- time: 30 minutes