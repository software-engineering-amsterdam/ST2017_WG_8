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


-------------------------------------------------------------------------------------
-- 1. a
left1, right1 :: Int -> Int
left1 = \x -> sum(map (^2) [0..x])
right1 = \x -> (((x*(x+1)) * (2*x+1)) `div` 6)

check1 = quickCheck (\x -> x >= 0 --> left1 x == right1 x)


-------------------------------------------------------------------------------------
-- 1. b
left2, right2 :: Int -> Int
left2 = \x -> sum(map (^3) [0..x])
right2 = \x -> ((x*(x+1))`div` 2)^2

check2 = quickCheck (\x -> x >= 0 --> left2 x == right2 x)


-------------------------------------------------------------------------------------
-- 2.
listLength :: Int -> Int
listLength = \x -> 2^(length [1..x])
subsets :: Int -> Int
subsets = \x -> length(subsequences [1..x])

check3 = quickCheck (\x -> listLength x == subsets x)
-- Got stuck after 50 tests

check3lim = quickCheck (\x -> x <= 20 --> listLength x == subsets x)


-------------------------------------------------------------------------------------
-- 3.
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

factorial :: Int -> Int
factorial = \x -> product[1..x]

check4 = quickCheck (\x -> length(perms[1..x])  == factorial x)
-- Got stuck after 15 tests

check4lim = quickCheck (\x -> x <= 10 --> length(perms[1..x])  == factorial x)


-------------------------------------------------------------------------------------
-- 4.
reversal :: Integer -> Integer
reversal = read . reverse . show

reversalCheck = quickCheck (\x -> x == reversal(reversal x))
-- Doesn't work, -x and 1000 -> 0001 -!>
-- Solution, limit the xs to prime numbers

reversePrimes = takeWhile (< 10000) (filter (\x -> prime (reversal x)) primes)


-------------------------------------------------------------------------------------
-- 5.

nextPrime :: Integer -> Integer
nextPrime x = if prime (x+1) then (x+1) else nextPrime(x+1)

first101 = take 101 primes

slideRange :: [Integer] -> [Integer]
slideRange = \x -> (tail x) ++ [nextPrime(last x)]

checkAndSlide :: [Integer] -> (Integer, [Integer])
checkAndSlide x = if prime(sum(x)) then (sum(x),x) else checkAndSlide(slideRange(x))

-------------------------------------------------------------------------------------
-- 6. 
-- Create a list of consecuitve primes up to 'x'
limitedPrimes :: Integer -> [Integer]
limitedPrimes = \x -> takeWhile (< x)(primes)

-- Check if [p1 * p2 * .. * px] + 1 is a prime 
statement :: Integer -> Bool
statement = \x -> prime(product(limitedPrimes x) + 1) 

-- Check with Quickcheck
check6 = quickCheck(statement)


smallestTrue = \x -> if statement x == False then x else smallestTrue (x+1)
-- Smallest number is 13


-------------------------------------------------------------------------------------
-- 7.

-- First produces a list of digits in reversed order (1024 -> [4, 2, 0, 1])
intToDigits :: Integer -> [Integer]
intToDigits = \x -> if (x < 10) then [x] else mod x 10 : intToDigits(div x 10)

-- Then double every second digit 
doubleDigits :: [Integer] -> [Integer]
doubleDigits [] = []
doubleDigits [x] = [x]
doubleDigits (x:y:z) = x:(y*2):doubleDigits z

-- Correct every second digit to fit the Luhn rules
correctDigits :: [Integer] -> [Integer]
correctDigits [] = []
correctDigits [x] = [x]
correctDigits (x:y:z) = if (y >= 10) then x:(y-9):correctDigits z else x:y:correctDigits z

luhn :: Integer -> Bool
luhn = \x -> sum(correctDigits(doubleDigits(intToDigits x))) `mod` 10 == 0

checkFirstN :: [Integer] -> Integer -> [Integer]
checkFirstN x 0 = []
checkFirstN [] y = []
checkFirstN x y = (head x) : (checkFirstN (tail x) (y-1))

-- From https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

checkCard :: Integer -> [Integer] -> Integer -> Bool
checkCard x y 0 = False
checkCard x y z= elem (fromDigits( checkFirstN (intToDigits(reversal x)) z)) y

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress x =  checkCard x [34, 37] 2
isMaster x | checkCard x [2221..2720] 4 = True
           | otherwise = checkCard x [51..55] 2
isVisa x = checkCard x [4] 1


-------------------------------------------------------------------------------------
-- 8.
data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)
 
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool

accusers :: Boy -> [Boy]

guilty, honest :: [Boy]

accuses Matthew x = not (x==Matthew) && not (x==Carl)

accuses Peter x = x == Matthew || x == Jack

accuses Jack x = not(accuses Matthew x) && not(accuses Peter x) 

accuses Arnold x = (accuses Matthew x) xor (accuses Peter x)

accuses Carl x = not(accuses Arnold x)

-}

main = print (checkAndSlide first101)