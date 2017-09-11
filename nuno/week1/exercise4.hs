module Lab1 where
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

-- To test the reversal function I could check if the reverse of the reverse of a number is the same number as the original
    -- This wouldn't work for n = 10 because the reversal of that n is 01 = 1, and the reversal of 1 is 1 != 10. 
    -- That's why I use numbers that are not multiples of 10 (last digit is not zero)
goodNForReversal :: Integer -> Bool
goodNForReversal n = mod n 10 /= 0

testreverse = \ n -> goodNForReversal n ==> reversal ( reversal n) == n

main                = print (findSpecialPrimes 10000)

--time: 4 minutes
