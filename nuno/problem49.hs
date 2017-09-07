module Lab1 where
import Data.List
import Test.QuickCheck  

convertToDigits :: Integer -> [Integer]
convertToDigits 0 = []
convertToDigits n = convertToDigits (div n 10) ++ [(mod n 10)]

convertToNumber :: [Integer] -> Integer
convertToNumber [] = 0
convertToNumber xs = 10 * (convertToNumber (init xs))+ (last xs)

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
     where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]


specialEqDistPrimes = [ [x,y,z] | x <- [1000..10000], y <- [x+1..10000], z <- [y+y-x], x /= 1487, -- x, y and z are 4-digit numbers
                                 prime x, prime y, prime z,                 -- equally distant primes
                                 elem y (map convertToNumber (permutations (convertToDigits x))), -- y is a permutation of x
                                 elem z (map convertToNumber (permutations (convertToDigits x)))] -- z is a permutation of x

-- Returns the answer where the 3 permutations are concatenated together.
formatAnswer :: [Integer] -> Integer
formatAnswer specialPs = let perm = sort specialPs in (((head perm)*10000) + (head ( tail perm)))*10000 + (last perm)

-- You should compile the code before running, otherwise it will take around a minute to give an answer
main = print (formatAnswer (head specialEqDistPrimes))

-- time: 45 minutes