module Lab1 where
import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..]

-- In theory we shouldn't test the answer since prior to reaching this solution we try all the smaller lists
-- We could check if all the 101 numbers really are primes or if they really add up to the right number

nextPrime :: Integer -> Integer
nextPrime x = if prime (x+1) then (x+1) else nextPrime(x+1)

first101 = take 101 primes

slideRange :: [Integer] -> [Integer]
slideRange = \x -> (tail x) ++ [nextPrime(last x)]

checkAndSlide :: [Integer] -> (Integer, [Integer])
checkAndSlide x = if prime(sum(x)) then (sum(x),x) else checkAndSlide(slideRange(x))

main = print(checkAndSlide first101)