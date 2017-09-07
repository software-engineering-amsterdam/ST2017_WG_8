module Lab1 where
import Data.List
import Test.QuickCheck  

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
     where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..2000000] -- I just want the primes below 2 million

-- Running this in interpreted mode might take a few seconds
main = print (sum primes)

--time: 4 minutes