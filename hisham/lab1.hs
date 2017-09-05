module Lab1 where
import Data.List
import Test.QuickCheck

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


-- Assignment 1 [1,5 hours]

-- Exercise 2 [1 hour]
f21, f22 :: Int -> Int
f21 = \n -> sum (map (^2) [0..n])
f22 = \n -> n*(n+1) * (2*n+1) `div` 6

testf2 = quickCheckResult (\n -> n >= 0 --> f21 n == f22 n)

-- Exercise 3 [30 mins]
f31, f32 :: Int -> Int
f31 = \n -> sum (map (^3) [0..n])
f32 = \n -> (^2) (n*(n+1) `div` 2)

testf3 = quickCheckResult (\n -> n >= 0 --> f31 n == f32 n)


-- Assignment 2 [30 mins]

-- Exercise 4 [30 mins]
-- Answer: It's hard to test because of its exponential growth.
f4 :: Int -> Bool
f4 = \n -> 2^(length [1..n]) == length (subsequences [1..n])

testf4 = quickCheck f4


-- Assignment 3 [30 mins]

-- Exercise 5 [30 mins]
f51, f52 :: Int -> Int
f51 = \n -> length (permutations [1..n])
f52 = \n -> product[1..n]

testf5 = quickCheck (\n -> n >= 1 --> f51 n == f52 n)


-- Assignment 4 [1,5 hours]
reversal :: Integer -> Integer
reversal = read . reverse . show

reversalPrimes :: [Integer]
reversalPrimes = takeWhile (<10000) $ filter (prime.reversal) primes


-- Assignment 5


-- Assignment 6 [2 hours]


-- Assignment 7