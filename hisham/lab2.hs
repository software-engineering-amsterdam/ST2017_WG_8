module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)


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