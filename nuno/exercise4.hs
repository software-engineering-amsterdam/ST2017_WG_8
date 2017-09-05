import Data.List
import Test.QuickCheck  

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
     where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

reversal :: Integer -> Integer
reversal = read . reverse . show

isReversal :: Integer -> Bool
isReversal n = n == reversal n

findSpecialPrimes :: Integer -> [Integer]
findSpecialPrimes n = filter prime (filter isReversal [2..n])

main                = print (findSpecialPrimes 10000)

-- To test the reversal function I would have to be able to check if the function was giving the correct results for a certain number. But that means
-- 			I had to program another reversal function to test this reversal function. If the tests would all pass, it could be that my function
-- 			was as incorrectly implemented as the first reversal function.
--time: 4 minutes
