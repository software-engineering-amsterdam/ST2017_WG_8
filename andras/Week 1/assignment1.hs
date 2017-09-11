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
-- 1. a Time spent: ~ 15 minutes

left1, right1 :: Int -> Int
left1 = \x -> sum(map (^2) [0..x])
right1 = \x -> (((x*(x+1)) * (2*x+1)) `div` 6)

check1a = quickCheck (\x -> x >= 0 --> left1 x == right1 x)

-- Result 1a:
-- main = check1a

-------------------------------------------------------------------------------------
-- 1. b Time spent: ~ 15 minutes

left2, right2 :: Int -> Int
left2 = \x -> sum(map (^3) [0..x])
right2 = \x -> ((x*(x+1))`div` 2)^2

check1b = quickCheck (\x -> x >= 0 --> left2 x == right2 x)

-- Result 1b:
-- main = check1b

-------------------------------------------------------------------------------------
-- 2. Time spent: ~ 15 minutes

listLength :: Int -> Int
listLength = \x -> 2^(length [1..x])
subsets :: Int -> Int
subsets = \x -> length(subsequences [1..x])

check2 = quickCheck (\x -> listLength x == subsets x)
-- Got stuck after 50 tests

check2lim = quickCheck (\x -> x <= 20 --> listLength x == subsets x)

-- Result 2:
-- main = check2
{- This property is hard to check due to the exponential O complexity of working with permutations 
   If we limit the range of test cases (as seen in check2lim) we can improve the performance although this somewhat defeats the purpose 
   The solution is not a mathematical proof, but rather a test of subsequences with a know mathematical formula 
-}

-------------------------------------------------------------------------------------
-- 3. Time spent: ~ 15 minutes

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

factorial :: Int -> Int
factorial = \x -> product[1..x]

check3 = quickCheck (\x -> length(perms[1..x])  == factorial x)
-- Got stuck after 15 tests

check3lim = quickCheck (\x -> x <= 10 --> length(perms[1..x])  == factorial x)

-- Result 3: 
-- main = check3
-- main = check3lim
{- Again this property is hard to check due to the exponentially growing complexity 
   If we limit the range of test cases (as seen in check3lim) we can improve the performance although this somewhat defeats the purpose
   Again, we can't really use the code as a mathematical proof, we just verify that the function works as we expect it to work  
-}


-------------------------------------------------------------------------------------
-- 4. Time spent: ~ 30 minutes
reversal :: Integer -> Integer
reversal = read . reverse . show

-- By reversing a reversed number, we should get the original number (given that the function works well)
reversalCheck = quickCheck (\x -> prime(x) --> x == reversal(reversal x))
-- We're using only primes to avoid problem with negative numbers and numbers ending with 0

reversePrimes = takeWhile (< 10000) (filter (\x -> prime (reversal x)) primes)

-- Result 4:
-- main = reversal check, reversePrimes

-------------------------------------------------------------------------------------
-- 5. Time spent: ~ 30 minutes

nextPrime :: Integer -> Integer
nextPrime x = if prime (x+1) then (x+1) else nextPrime(x+1)

-- Create a "frame" of the first 101 primme numbers
first101 = take 101 primes

-- Slide the frame by one
slideRange :: [Integer] -> [Integer]
slideRange = \x -> (tail x) ++ [nextPrime(last x)]

-- Slide frame until correct 101 number found
checkAndSlide :: [Integer] -> (Integer, [Integer])
checkAndSlide x = if prime(sum(x)) then (sum(x),x) else checkAndSlide(slideRange(x))

-- Result 5: 
-- main = checkAndSlide first101

-------------------------------------------------------------------------------------
-- 6. Time spent: ~ 1 hour

-- Create a list of consecuitve primes up to 'x'
limitedPrimes :: Integer -> [Integer]
limitedPrimes = \x -> takeWhile (< x)(primes)

-- Check if [p1 * p2 * .. * px] + 1 is a prime 
statement :: Integer -> Bool
statement = \x -> prime(product(limitedPrimes x) + 1) 

-- Check with Quickcheck
check6 = quickCheck(statement)


-- Smallest number is 17
smallestTrue = take 1 (filter (\x -> statement x == False) primes)


-- Result 6: 
-- main = print(smallestTrue)

-------------------------------------------------------------------------------------
-- 7. Time spent: ~ 2 hours

-- First produces a list of digits in reversed order (1024 -> [4, 2, 0, 1])
intToDigits :: Integer -> [Integer]
intToDigits = \x -> if (x < 10) then [x] else mod x 10 : intToDigits(div x 10)

-- Then double every second digit 
doubleDigits :: [Integer] -> [Integer]
doubleDigits [] = []
doubleDigits [x] = [x]
doubleDigits (x:y:z) = x:(y*2):doubleDigits z

-- Correct every second digit to fit the Luhn rules
fixDigits :: [Integer] -> [Integer]
fixDigits [] = []
fixDigits [x] = [x]
fixDigits (x:y:z) = if (y >= 10) then x:(y-9):fixDigits z else x:y:fixDigits z

luhn :: Integer -> Bool
luhn = \x -> sum(fixDigits(doubleDigits(intToDigits x))) `mod` 10 == 0

checkFirstN :: [Integer] -> Integer -> [Integer]
checkFirstN x 0 = []
checkFirstN [] y = []
checkFirstN x y = (head x) : (checkFirstN (tail x) (y-1))

-- From https://stackoverflow.com/questions/1918486/convert-list-of-integers-into-one-int-like-concat-in-haskell
fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

checkDigits :: Integer -> [Integer] -> Integer -> Bool
checkDigits x y 0 = False
checkDigits x y z= elem (fromDigits( checkFirstN (intToDigits(reversal x)) z)) y

isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress x =  luhn x && checkDigits x [34, 37] 2
isMaster x | luhn x && checkDigits x [2221..2720] 4 = True
           | otherwise =  luhn x && checkDigits x [51..55] 2
isVisa x = luhn x && checkDigits x [4] 1


-- A list of a few valid card numbers (all of them are either American Express, Mastercard, or Visa) 
validCards = [4111111111111111, 5500000000000004, 340000000000009, 378282246310005, 371449635398431, 5555555555554444, 5105105105105100, 4012888888881881, 4222222222222]


check7 = quickCheck (all luhn validCards)


-- Result 7: 
-- main = print(luhn <card number>)
-- main = print(isAmericanExpress <card number>)
-- main = print(isMaster <card number>)
-- main = print(isVisa <card number>)
-- main = check7


-------------------------------------------------------------------------------------
-- 8. Time spent: ~ 2 hours

data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)
 
boys = [Matthew, Peter, Jack, Arnold, Carl]


accuses :: Boy -> Boy -> Bool
accuses Matthew x = (x /= Matthew) && (x /= Carl)
accuses Peter x = (x == Matthew) || (x == Jack)
accuses Jack x = not ( (accuses Matthew x) || (accuses Peter x))

-- Based on: p xor q = (p || q) && (!p && !q)  
accuses Arnold x =  (((accuses Matthew x) || (accuses Peter x)) && (not(accuses Matthew x) && not(accuses Peter x)))

accuses Carl x = not (accuses Arnold x)

-- Create a list of all the accusers of boy x
accusers :: Boy -> [Boy]
accusers x = filter (\y -> accuses y x) boys

-- If someone's accused by more than 2 people they are considered guilty
guilty :: [Boy]
guilty = filter (\x -> length(accusers x) > 2) boys

notGuilty :: [Boy]
notGuilty = filter (\x -> notElem x guilty) boys

-- A boy made a honest statement if he accused the guilty boys and didn't acused the non-guilty boys
honest :: [Boy]
honest = nub [x | x <- boys, y <- guilty, z <- notGuilty, accuses x y && not (accuses x z)]

-- Result 8:
-- main = guilty
-- main = honest


-------------------------------------------------------------------------------------
-- Bonus 1.


-------------------------------------------------------------------------------------
-- Bonus 2. Time spent: ~ 10 minutes

primeSum = sum(takeWhile( < 2000000) primes)

-- Result Bonus 2:
-- main = primeSum


-------------------------------------------------------------------------------------
-- Bonus 2.


main = print("Please check individual excercises for the results by uncommenting the appropriate lines of code")