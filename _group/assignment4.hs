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

reversal :: Integer -> Integer
reversal = read . reverse . show

reversalCheck = quickCheck (\x -> prime(x) --> x == reversal(reversal x))
-- We're using only primes to avoid problem with negative numbers and numbers ending with 0

reversePrimes = takeWhile (< 10000) (filter (\x -> prime (reversal x)) primes)