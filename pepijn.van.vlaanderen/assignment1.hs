module Lab1 where
import Data.List
import Test.QuickCheck

-- Assignment 1

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

infix 1 --> 
 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q
 
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

{- 
Excercise 1 - +/- 15 min.
-}
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

{- 
Excercise 2 - +/- 20 min.
The test takes a long time, so it seems hard to test this.
That is because of the exponential grow using the subsequence function and the 2^.
No mathematical fact, but the implementation of subsequences against its specification.
-}
finite_set :: Int -> Bool
finite_set = \n -> length (subsequences [1..n]) == 2^(length [1..n])

finite_set_test = quickCheck finite_set

{- 
Excercise 3
The test takes a long time, so it seems hard to test this.
That is because permutations are exponentially growing with the list size.
Again we check the implementation, this time for permutations.
-}
perm_test = quickCheck (\n -> length (permutations [1..n]) == product [1..n])

{-
Excercise 4
-}
reversal :: Integer -> Integer
reversal = read . reverse . show

{- 
Reverse does not work for negative numbers and does not work for numbers with trailing zero's (creates leading zero's on reverse).
So just test the reversal function on primes.
-}
reversal_test = quickCheck (\n -> prime n --> n == reversal (reversal n))

reverse_primes :: [Integer]
reverse_primes = takeWhile (< 10000) (filter (prime.reversal) primes)

{-
Exercise 5
-}