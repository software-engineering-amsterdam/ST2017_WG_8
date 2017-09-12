module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

-- A = 10, Z = 35
transformLetter :: Char -> [Char]
transformLetter c = if c >= 'A' && c <= 'Z' then show (35 - (ord 'Z') + (ord c)) else [c]

moveFourToEnd :: [Char] -> [Char]
moveFourToEnd (x1:x2:x3:x4:xs) = xs++[x1]++[x2]++[x3]++[x4]

transformAllLetters :: [Char] -> [Char]
transformAllLetters [] = []
transformAllLetters (x:xs) = (transformLetter x)++(transformAllLetters xs)

iban :: String -> Bool
iban s = (length s) > 4 && (length s) <= 34 && mod (read (transformAllLetters (moveFourToEnd s))) 97 == 1

goodExamples :: [String]
goodExamples = ["AL47 2121 1009 0000 0002 3569 8741","AD12 0001 2030 2003 5910 0100",
                "AT61 1904 3002 3457 3201","AZ21 NABZ 0000 0000 1370 1000 1944",
                "BH67 BMAG 0000 1299 1234 56","BE62 5100 0754 7061",
                "BA39 1290 0794 0102 8494","BG80 BNBG 9661 1020 3456 78",
                "HR12 1001 0051 8630 0016 0","CY17 0020 0128 0000 0012 0052 7600",
                "CZ65 0800 0000 1920 0014 5399","DK50 0040 0440 1162 43"]

-- Returns the converted int to a string with 2 digits
format2Digits :: Int -> String
format2Digits n = let converted = show n in if (length converted) == 1 then "0"++converted else converted

badExamples :: [String]
badExamples = ["PT"++(format2Digits x)++"TEST" | x <- [0..99], x /= 31]

testExamples :: [String] -> Bool -> Bool
testExamples examples supoosed = all (\x -> (iban (filter (\y -> y /= ' ') x)) == supoosed) examples

-- Checks if testing function "iban" with good iban numbers returns True for all and if testing the function "iban" with bad iban returns False
testIbans :: Bool
testIbans = testExamples goodExamples True && testExamples badExamples False

main = print testIbans

-- time: 45 minutes