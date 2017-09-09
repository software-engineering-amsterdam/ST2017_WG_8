module Lab1 where
import Data.List
import Test.QuickCheck  

-- Assignment euler9 #Time: 30 mins

answers = [a*b*c | a <- [0..1000], b <- [a+1..1000], c <- [b+1..1000], a+b+c == 1000, a^2 + b^2 == c^2]

-- Running this in interactive Haskell might take a few seconds
main = print (head (answers))