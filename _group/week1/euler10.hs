module Lab1 where
import Data.List
import Test.QuickCheck  

-- Assignment euler10 #Time: 4 mins

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
     where xs = takeWhile (\ y -> y^2 <= n) primesBelow2M

primesBelow2M :: [Integer]
primesBelow2M = 2 : filter prime [3..2000000] -- I just want the primes below 2 million

-- Running this in interpreted mode might take a few seconds
main = print (sum primesBelow2M)