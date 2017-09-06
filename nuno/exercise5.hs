module Lab1 where
import Data.List
import Test.QuickCheck  

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
     where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- https://stackoverflow.com/questions/19198744/compute-next-prime-number-in-haskell
nextPrime :: Integer -> Integer
nextPrime n | prime n = n 
            | otherwise = nextPrime (n+1)

-- Returns the x next primes after n. n must already be a prime
nextPrimes :: Integer -> Integer -> [Integer]
nextPrimes 0 n = [n]
nextPrimes x n = n : nextPrimes (x-1) (nextPrime (n+1))

-- Calculates the the sum of 101 consecutive primes, starting with n
sumConsecutive :: Integer -> Integer
sumConsecutive n = sum (nextPrimes 100 n)

-- Recursively finds the smallest prime that holds the property of being the sum of 101 consecutive primes by continuosly increasing n,
-- 		which will be the first prime of the 101 consecutive primes
findSmallestSpecialPrime :: Integer -> Integer
findSmallestSpecialPrime n = if prime (sumConsecutive n) then sumConsecutive n else findSmallestSpecialPrime (nextPrime (n+1))

smallestSpecialPrime :: Integer
smallestSpecialPrime = findSmallestSpecialPrime 2

main                = print smallestSpecialPrime

-- I'm not sure how could my answer be checked.
-- time: 20 minutes