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
iban s = mod (read (transformAllLetters (moveFourToEnd s))) 97 == 1

main = print (iban "GB82WEST12345698765432")

-- time: 25 minutes for now