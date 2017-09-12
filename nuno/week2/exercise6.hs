import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

rotateChars :: Char -> Int -> Char
rotateChars c n = if (n == 0 && c > 'z') || (n == 1 && c > 'Z') then chr( (ord c)-26) else c

charRot13 :: Char -> Char
charRot13 c = if c >= 'A' && c <= 'Z' then rotateChars (chr ((ord c)+13)) 1 else if c >= 'a' && c <= 'z' then rotateChars (chr ((ord c)+13)) 0 else c

rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 (x:xs) = (charRot13 x):(rot13 xs)

-- Possible properties: rot13 (rot13 (x)) == x                

main = print (rot13 "ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwzyz!")

-- time: 20 minutes