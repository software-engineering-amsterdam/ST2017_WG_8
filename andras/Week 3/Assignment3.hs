import Data.List
import System.Random
import Test.QuickCheck
import System.Random
import Lecture3


----------------------------------------------------------
-- 1)


contradiction :: Form -> Bool
contradiction f = any (\ v -> not $ evl v f) (allVals f)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- | logical entailment 
entails :: Form -> Form -> Bool
entails fp fq = all (\ v -> if evl v fp then evl v fq else True) (allVals fp) 

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv fp fq = all (\ v ->  evl v fp == evl v fq) (allVals fp) 

-- Examples from the workshop
conCheck = contradiction $ Cnj [p, Neg p]

tauCheck = contradiction $ Dsj [p, Neg p]

entCheck = entails p $ Dsj [p, q]

equCheck = equiv p p


checkBatch1 :: IO()
checkBatch1 = do
    print("Contradiction: ")
    if conCheck then print("Passed") else print("Failed")
    print("Tautology: ")
    if tauCheck then print("Passed") else print("Failed")
    print("Entailment: ")
    if entCheck then print("Passed") else print("Failed")
    print("Equivalence: ")
    if equCheck then print("Passed") else print("Failed")


-- Result 1:
-- main = checkBatch1

----------------------------------------------------------
-- 2)

-- To test parse I generate formulas from an [Int], this way parse can be tested with an arbitrary number of random formulas generated

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   xs <- getIntL k (n-1)
   return (x:xs)

genIntList :: Int -> Int -> IO [Int]
genIntList kRange nRange = do 
  k <- getRandomInt kRange
  n <- getRandomInt nRange
  getIntL k n


-- Create an [Int], this will be the seed for the formula
createSeed ::  IO [Int]
createSeed = do
    x <- getRandomInt 10
    xs <- getIntL 3 (x)
    return xs


-- Turns a [Int] seed into a valid Form in String format ready to be processed by parse
makeFormula :: [Int] -> String
makeFormula [] =  show 0
makeFormula [x] = show x
makeFormula (x:xs)
           | x == 0 = "-"++(makeFormula xs)  -- Neg p
           | x == 1 = "+("++(makeFormula xs)++" "++(show $ head xs)++")"  -- Disjunction p q
           | x == 2 = "*("++(makeFormula xs)++" "++(show $ head xs)++")"  -- Conjunction p q                      -- 
           | x == 3 = "("++(makeFormula xs)++"==>"++(show $ head xs)++")" -- Entailment
           | x == 4 = "("++(makeFormula xs)++"<=>"++(show $ head xs)++")" -- Equivalence
           | otherwise = ""
           

-- Just to see if makeFormula works as expected
checkFormula :: IO()
checkFormula = do
    x <- createSeed
    print (makeFormula x)
    print (makeFormula x)
    print (makeFormula x)


-- Limit quickCheck to a list of numbers between 0 and 4 ()
genPos :: Gen Int
genPos = abs `fmap` (arbitrary :: Gen Int) `suchThat` (\x -> (x >= 0 && x < 5))

genListOfPos :: Gen [Int]
genListOfPos = listOf genPos

-- parse works as expected if for any random, valid formula the output will only contain one element, the correctly parsed formula 
parseProperty :: [Int] -> Property
parseProperty xs = length xs < 30 ==> length (parse (makeFormula xs)) == 1

checkParse :: IO()
checkParse = do
    quickCheck $ forAll genListOfPos $ parseProperty

-- Result 2:
-- main = checkParse

----------------------------------------------------------
-- 3)

-- Check if arrowfree fulfills the postconditions

arrowProperty :: [Int] -> Property
arrowProperty xs = length xs < 30 ==> not(elem '=' $ (show . arrowfree . head . parse . makeFormula) xs) && equiv ((head . parse . makeFormula) xs) ((arrowfree . head . parse . makeFormula) xs )

checkArrowfree :: IO()
checkArrowfree = do
    quickCheck $ forAll genListOfPos $ arrowProperty

-- Check if nnf fulfills the postconditions
nnfProperty :: [Int] -> Property
nnfProperty xs = length xs < 30 ==> not(isInfixOf "-(" $ (show . nnf . arrowfree . head . parse . makeFormula) xs) 

checkNnf :: IO()
checkNnf = do
    quickCheck $ forAll genListOfPos $ nnfProperty

toCNF :: Form ->  Form
toCNF f = (nnf . arrowfree) f

stringToCNF :: String -> Form
stringToCNF s = (toCNF . head . parse) s


----------------------------------------------------------
-- 4)


main = checkNnf