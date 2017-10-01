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
Excercise 1 (30 min)
-}
-- Workshop exercise 2
ws21, ws22 :: Int -> Int
ws21 n = sum (map (^2) [0..n])
ws22 n = n*(n+1)*(2*n+1) `div` 6

test_ws2 = quickCheckResult (\n -> n >= 0 --> ws21 n == ws22 n)

-- Workshop exercise 3
ws31, ws32 :: Int -> Int
ws31 n = sum (map (^3) [0..n])
ws32 n = (n*(n+1) `div` 2)^2

test_ws3 = quickCheckResult (\n -> n >= 0 --> ws31 n == ws32 n)

{-
Excercise 2 (30 min)
The test takes a long time, so it seems hard to test this.
That is because of the exponential grow using the subsequence function and the 2^.
No mathematical fact, but the implementation of subsequences against its specification.
-}
finiteSet :: Int -> Bool
finiteSet n = length (subsequences [1..n]) == 2^length [1..n]
finiteSetTest = quickCheck finiteSet

{-
Excercise 3 (20 min)
The test takes a long time, so it seems hard to test this.
That is because permutations are exponentially growing with the list size.
Again we check the implementation, this time for permutations.
-}
permTest = quickCheck (\n -> length (permutations [1..n]) == product [1..n])

{-
Excercise 4 (60 min)
-}
reversal :: Integer -> Integer
reversal = read . reverse . show

{-
Reverse does not work for negative numbers and does not work for numbers with trailing zero's (creates leading zero's on reverse).
So just test the reversal function on primes.
-}
reversalTest = quickCheck (\n -> prime n --> n == reversal (reversal n))

reversePrimes :: [Integer]
reversePrimes = takeWhile (< 10000) (filter (prime.reversal) primes)

{-
Exercise 5 (20 min)
Process function that generates from a given list a list with sublists from n length.
In our case i generates sublists with 101 primes.

Then use filter to get only the sublists where the sum of it is also a prime.
And take the first element with head and then the sum for the prime value.

The answer is correct based upon the working of the core haskell functions.
-}
subLists :: Int -> [a] -> [[a]]
subLists n xs = take n xs : subLists n (tail xs)

sumConsecutive101 = sum (head (filter (prime.sum) (subLists 101 primes)))

{-
Exercise 6 (30 min)
-}
primeList :: Int -> [a] -> [[a]]
primeList n list = take n list : primeList (n+1) list

counterExamples :: [([Integer],Integer)]
counterExamples = [ (ps,product ps + 1) |
                          ps <- primeList 1 primes,
                          not $ prime (product ps + 1) ]

counterExample :: ([Integer], Integer)
counterExample = head counterExamples

{-
Exercise 7 (60 min)
-}
-- https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
intToDigits :: Integer -> [Integer]
intToDigits 0 = []
intToDigits n = intToDigits (div n 10) ++ [mod n 10]

revDigits :: Integer -> [Integer]
revDigits n = reverse (intToDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:z) = if (y*2) >= 10
                           then x:(y*2-9):doubleEveryOther z
                           else x:(y*2):doubleEveryOther z

isValid :: Integer -> Bool
isValid n = sum (doubleEveryOther (revDigits n)) `mod` 10 == 0

validCards = [4111111111111111, 5500000000000004, 340000000000009, 378282246310005, 371449635398431, 5555555555554444, 5105105105105100, 4012888888881881, 4222222222222]
