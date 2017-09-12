module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Rot13 specification: Given a string s, transform every letter (character from 'a' to 'z' or 'A' to 'Z') in s by replacing it with the letter that is 
--      13 positions ahead of c in the alphabet, wrapping back to the beginning if necessary. Using the basic Latin alphabet (26 letters) is necessary
--      in order to maintain the property that applying Rot13 twice to a string will result in the same string.

rotateChars :: Char -> Int -> Char
rotateChars c n = if (n == 0 && c > 'z') || (n == 1 && c > 'Z') then chr( (ord c)-26) else c

charRot13 :: Char -> Char
charRot13 c = if c >= 'A' && c <= 'Z' then rotateChars (chr ((ord c)+13)) 1 else if c >= 'a' && c <= 'z' then rotateChars (chr ((ord c)+13)) 0 else c

rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 (x:xs) = (charRot13 x):(rot13 xs)

-- Applying rot13 twice will give the same string
prop1 :: [Char] -> Bool
prop1 s = rot13 (rot13 s) == s

-- Applying rot13 then reversing will give the same string as if we applied rot13 to the reversed list
prop2 :: [Char] -> Bool
prop2 s = rot13 (reverse s) == reverse (rot13 s)

-- The result of rot13 of a string will have the same length as the original string
prop3 :: [Char] -> Bool
prop3 s = length s == length (rot13 s)

-- Non alphabetic characters will not change when applying rot13
prop4 :: [Char] -> Bool
prop4 s = let nonAlpha = filter (\x -> x < 'A' || x > 'z' || (x > 'Z' && x < 'a')) s in nonAlpha == (rot13 nonAlpha)

main = quickCheck prop1

-- time: 40 minutes