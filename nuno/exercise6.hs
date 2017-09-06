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

-- Checks what is the smallest counter example for the statement of exercise6, starting from the prime n and assuming that the product is already at p
smallestCounter :: Integer -> Integer -> Integer
smallestCounter p n = if not (prime ((p*n)+1)) then n else smallestCounter (p*n) (nextPrime (n+1))

-- Returns the smallest prime number where 1 plus the product of all prime numbers before him and including him is not prime
smallestCounterAux :: Integer
smallestCounterAux = smallestCounter 1 2

main                = print smallestCounterAux

--time: 15 minutes