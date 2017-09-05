import Data.List
import Test.QuickCheck 

-- Convert a number to a list of digits to be easier to apply the formula
convertNumberToDigits :: Integer -> [Integer]
convertNumberToDigits 0 = []
convertNumberToDigits n = convertNumberToDigits (div n 10) ++ [(mod n 10)]

-- Convert a list of digits to a number
convertDigitsToNumber :: [Integer] -> Integer
convertDigitsToNumber [] = 0
convertDigitsToNumber xs = 10 * (convertDigitsToNumber (init xs))+ (last xs)

-- Receives a list of digits of even length and returns a list modifying the digits to by 2 accordingly to Luhn's algorithm
applyLuhnAux :: [Integer] -> [Integer]
applyLuhnAux [] = []
applyLuhnAux (x1:x:xs) = let x2 = x1*2
                         in if x2 > 9 
                                then ((x2-9) : x : applyLuhnAux xs) 
                                else (x2 : x : applyLuhnAux xs)


applyLuhn :: [Integer] -> Bool
applyLuhn n = mod (sum (applyLuhnAux n)) 10 == 0

-- luhnAlg makes the length of the list an even number, by adding a 0 if necessary to the beginning of the list which wont affect the results
luhnAlg :: Integer -> Bool
luhnAlg n = let digits = convertNumberToDigits n 
            in if mod (length digits) 2 == 1 
                then applyLuhn (0 : digits) 
                else applyLuhn digits

-- Returns first n digits of a number to check IIN
firstNDigits :: Int -> Integer -> Integer
firstNDigits n number = convertDigitsToNumber (take n (convertNumberToDigits number))

-- Characteristics: iin in (34,37) | length = 15 | luhnAlg
isAmericanExpress :: Integer -> Bool
isAmericanExpress n = let iin = firstNDigits 2 n 
                      in (iin == 34 || iin == 37) && 
                          length (convertNumberToDigits n) == 15 && 
                          luhnAlg n

-- Characteristics: iin is 4 | length in (13,16,19) | luhnAlg
isVisa :: Integer -> Bool
isVisa n = let len = length (convertNumberToDigits n) 
           in (firstNDigits 1 n) == 4 && 
              (elem len [13,16,19]) && 
              luhnAlg n

-- Characteristics: iin is in ([51..55], [2221..2720]) | length = 16 | luhnAlg
isMaster :: Integer -> Bool
isMaster n = 
    let iin  = firstNDigits 4 n; iin2 = firstNDigits 2 n
    in ((elem iin [2221..2720]) || (elem iin2 [51..55])) && 
        length (convertNumberToDigits n) == 16 && 
        luhnAlg n

-- Tests: Adding any number except 5 to any digit of a valid number will always result in an invalid number
--          373456789012344  is a valid american express card number
--          4512619341241    is a valid visa card number
--          5125192582591255 is a valid master card number

-- Generate good n's to test. We dont want n to be either 0 or 5, otherwise it could generate valid numbers
goodNs :: Int -> Int
goodNs n = let n1 = mod n 10 in if elem n1 [0,5] then n1+1 else n1

checkIfInvalidCard :: (Integer -> Bool) -> Int -> [Integer] -> [Integer] -> Bool
checkIfInvalidCard _ _ _ [] = True
checkIfInvalidCard prop toAdd validNumber (i:rest) = not (prop (convertDigitsToNumber ((take (fromIntegral i) validNumber) ++ 
                                                                                                [validNumber!!(fromIntegral i) + 2] ++ 
                                                                                                drop (fromIntegral (i+1)) validNumber )))
                                                && checkIfInvalidCard prop toAdd validNumber rest

-- Check if by adding number n to any digit of a valid number results in an invalid number as it should
checkIfInvalidAmEx :: Int -> Bool
checkIfInvalidAmEx n = checkIfInvalidCard isAmericanExpress n (convertNumberToDigits 373456789012344) [0..14]

checkIfInvalidVisa :: Int -> Bool
checkIfInvalidVisa n = checkIfInvalidCard isVisa n (convertNumberToDigits 373456789012344) [0..14]

checkIfInvalidMaster :: Int -> Bool
checkIfInvalidMaster n = checkIfInvalidCard isMaster n (convertNumberToDigits 373456789012344) [0..14]

exerciseSevenAmEx :: [Int] -> Bool
exerciseSevenAmEx xs = (isAmericanExpress 373456789012344) && all checkIfInvalidAmEx (map goodNs xs )

exerciseSevenVisa :: [Int] -> Bool
exerciseSevenVisa xs = (isVisa 4512619341241) && all checkIfInvalidVisa (map goodNs xs)

exerciseSevenMaster :: [Int] -> Bool
exerciseSevenMaster xs = (isMaster 5125192582591255) && all checkIfInvalidMaster (map goodNs xs)

--main = print (isAmericanExpress 373456789012344) -- returns True
--main = print (isVisa 4512619341241) -- returns True
--main = print (isMaster 5125192582591255) -- returns True
main = quickCheck exerciseSevenMaster

--time 1h30m
-- not done yet