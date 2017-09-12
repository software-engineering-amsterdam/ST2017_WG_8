
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 
 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-----------------------------------------------------------------------------------------
-- 1. Time spent: ~ 25 minutes (of which 15 minutes was fighting with IO [Float ])

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

checkRandom :: [Float] -> [Int]
checkRandom [] = [0, 0, 0, 0]
checkRandom x = [(length(filter(<=0.25) x)), (length(filter(\x -> x > 0.25 && x <= 0.5) x)), (length(filter(\x -> x > 0.5 && x <= 0.75) x)), (length(filter(\x -> x > 0.75 && x <= 1) x))]


{-

Result 1:
main :: IO()
main = do
            x <- probs 10000
            -- We assume that if a container has more/less than 2500 +- (2500 * 0.05) the distribution is not random enough
            print(if length(filter (\x -> x >= 2375 && x <= 2625) (checkRandom x)) == 4 then "Random distribution checks out" else "Distribution is not random")
-}


-----------------------------------------------------------------------------------------
-- 2. Time spent: ~ 15 minutes +  ~ 15 minutes for tests + batch 

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)



triangle :: Integer -> Integer -> Integer -> Shape 
triangle x y z | (x >= y + z || y > x + z || z > x + y || x <= 0 || y <= 0 || z <= 0) = NoTriangle
               | (x == y && y == z ) = Equilateral
               | (x == y || x == z || y == z) = Isosceles
               | (x^2 == y^2 + z^2 || y^2 == x^2 + z^2 || z^2 == x^2 + y^2) = Rectangular
               | otherwise = Other

-- These tests are a bit trivial since we're checking for the same conditions that are in the actual function, failing the test is virtually impossible, but technically they should be correct
-- A small difference is that we didn't need to specif that x, y, and z are > 0, since the first pattern matching statement should catch any other values, in the quickCheck we have to specify this each time
checkNo = quickCheck (\x y z -> (x >= y + z || y > x + z || z > x + y || x <= 0 || y <= 0 || z <= 0) --> triangle x y z == NoTriangle)
checkEq = quickCheck (\x y z -> (x == y && y == z && x > 0 && y > 0 && z > 0)  --> triangle x y z == Equilateral)
checkIso = quickCheck (\x y z -> (x == y || x == z || y == z) &&( x > 0 && y > 0 && z > 0) --> triangle x y z == Isosceles)
checkRec = quickCheck (\x y z -> (x^2 == y^2 + z^2 || y^2 == x^2 + z^2 || z^2 == x^2 + y^2) && ( x > 0 && y > 0 && z > 0) --> triangle x y z ==  Rectangular)
checkOther = quickCheck (\x y z -> (x >= y + z || y > x + z || z > x + y) && (x > 0 && y > 0 && z > 0) && not(x == y || x == z || y == z) && not(x^2 == y^2 + z^2 || y^2 == x^2 + z^2 || z^2 == x^2 + y^2) --> triangle x y z == NoTriangle)

checkBatch2 :: IO()
checkBatch2 = do
    print("Not-a-triangle check:")
    checkNo
    print("Equilateral check:")
    checkEq
    print("Isosceles check:")
    checkIso
    print("Rectangular check:")
    checkRec
    print("Other check:")
    checkOther

-- Result 2:
-- main = print(trinagle <Int1> <Int2> <Int3>)
-- main = checkBatch2

-----------------------------------------------------------------------------------------
-- 3. Time spent: ~ 20 minutes

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

x1s = stronger [-10..10](\ x -> even x && x > 3) even -- True
x1w = weaker [-10..10](\ x -> even x && x > 3) even -- False
-- (\ x -> even x && x > 3) is stronger than 'even'

x2s = stronger [-10..10](\ x -> even x || x > 3) even -- False
x2w = weaker [-10..10](\ x -> even x || x > 3) even -- True

x3s = stronger [-10..10](\ x -> (even x && x > 3) || even x) even -- True 
x3w = weaker [-10..10](\ x -> (even x && x > 3) || even x) even -- True

x4s = stronger [-10..10] even (\ x -> (even x && x > 3) || even x) -- True
x4w = weaker [-10..10] even (\ x -> (even x && x > 3) || even x) -- True

checkBatch3 :: IO()
checkBatch3 = do
    print("1")
    print("stronger")
    print(x1s)
    print("weaker")
    print(x1w)
    print("----------------------")
    print("2")
    print("stronger")
    print(x2s)
    print("weaker")
    print(x2w)
    print("----------------------")
    print("3")
    print("stronger")
    print(x3s)
    print("weaker")
    print(x3w)
    print("----------------------")
    print("4")
    print("stronger")
    print(x4s)
    print("weaker")
    print(x4w)
    print("----------------------")


-- Result 3:
-- main = checkBatch2

{- 
Order from strongest to weakest
Strong
---------------------
(\ x -> even x && x > 3)
(\ x -> even x || x > 3)
(\ x -> (even x && x > 3) || even x), even
---------------------
Weak
-}


-----------------------------------------------------------------------------------------
-- 4. Time spent: ~ 40 minutes
-- TODO: Properties, tests

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation x y = (length [x] == length [y]) && (all (\z -> z `elem` y) x) 


-- main = print(isPermutation [1, 2, 3] [3, 1, 2])


-----------------------------------------------------------------------------------------
-- 5. Time spent: ~ 40 minutes
-- TODO: Properties, QuickCheck

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = False
isDerangement x y = (isPermutation x y) && parsePermutation x y

parsePermutation :: Eq a => [a] -> [a] -> Bool
parsePermutation [] [] = True
parsePermutation x y = if (head x == head y ) then False else parsePermutation (tail(x)) (tail(y)) 


deran :: Eq a => [a] -> [[a]]
deran [] = []
deran x = filter (\y -> isDerangement x y) (permutations x)

-- Result 5:
-- main = print(isDerangement <list> <list>)
-- main = print(deran <list>)

-----------------------------------------------------------------------------------------
-- 6. Time spent: ~ 30 minutes
-- TODO: Specifications, quickCheck

-- To turn a char into its ROT13 equivalent I just map it from one list to the other
-- The '#' at the end of both list is for invalid (non-alphabetical) chars
normalAlph = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#"
rot13Alph  = "NOPQRSTUVWXYZABCDEFGHIJKLMnopqrstuvwxyzabcdefghijklm#"

-- If any char of the input string is not a letter, we replace it with '#'
sanitizeString :: String -> String
sanitizeString s = [(if notElem c normalAlph then '#' else c) | c <- s]

-- Turn a char into its ROT13 equivalent
findRot :: Char -> Char
findRot c = rot13Alph!!(head(elemIndices c normalAlph))

-- Transform each char of the input String
sentanceToRot :: String -> String
sentanceToRot s = [findRot c | c <- s]

-- Result 6:
-- main = print(sentanceToRot(sanitizeString <String>))

-----------------------------------------------------------------------------------------
-- 7. Time spent: ~ 50 minutes

alph = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- Step 1: Move the first 4 chars to the end
move4 :: String -> String
move4 s = (drop 4 s) ++ (take 4 s)

-- Step 2: Convert chars into integers (A -> 10, B -> 11, .. , Z -> 35)
convertLetters :: String -> String
convertLetters s = concat[maybeLetter c | c <- s]

maybeLetter :: Char -> String
maybeLetter c | elem c alph = show(((\(Just i)->i)(elemIndex c alph)) + 10)
              | otherwise = [c] 

-- Step 3: Check mod 97 == 1
iban :: String -> Bool
iban s = (length s <= 34) && (read(convertLetters(move4(s))) `mod` 97 == 1)

validIbans = ["AL47212110090000000235698741", "AD1200012030200359100100", "AT611904300234573201", "AZ21NABZ00000000137010001944", "BH67BMAG00001299123456", "BE62510007547061", "BA391290079401028494", "BG80BNBG96611020345678", "HR1210010051863000160", "CY17002001280000001200527600", "CZ6508000000192000145399", "DK5000400440116243", "EE382200221020145685", "FO9754320388899944", "FI2112345600000785", "FR1420041010050500013M02606", "GE29NB0000000101904917", "DE89370400440532013000", "GI75NWBK000000007099453", "GR1601101250000000012300695", "GL5604449876543210", "HU42117730161111101800000000", "IS140159260076545510730339", "IE29AIBK93115212345678", "IL620108000000099999999", "IT40S0542811101000000123456"]

main = print (all (iban) validIbans)
