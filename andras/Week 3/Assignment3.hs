import Data.List
import System.Random
import Test.QuickCheck
import System.Random
import Lecture3


----------------------------------------------------------
-- 1) Time spent ~ 1 hour


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
-- 2) Time spent ~ 2 hours

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
genParse :: Gen Int
genParse = choose (0, 4)

genListOfParse :: Gen [Int]
genListOfParse = listOf genParse

-- parse works as expected if for any random, valid formula the output will only contain one element, the correctly parsed formula 
parseProperty :: [Int] -> Property
parseProperty xs = length xs < 30 ==> length (parse (makeFormula xs)) == 1

checkParse :: IO()
checkParse = do
    quickCheck $ forAll genListOfParse $ parseProperty

-- Result 2:
-- main = checkParse

----------------------------------------------------------
-- 3) Time spent ~ 1 hour 

-- Functions reused from previous excercise/lecture: arrowfree, nnf, parse, makeFormula, equiv, genListOfParse

-- Check if arrowfree fulfills the postconditions (length is limited for runtimpe perforamnce improvement)
arrowProperty :: [Int] -> Property
arrowProperty xs = length xs < 30 ==> not(elem '=' $ (show . arrowfree . head . parse . makeFormula) xs) && equiv ((head . parse . makeFormula) xs) ((arrowfree . head . parse . makeFormula) xs )

checkArrowfree :: IO()
checkArrowfree = do
    quickCheck $ forAll genListOfParse $ arrowProperty

-- Check if nnf fulfills the postconditions
nnfProperty :: [Int] -> Property
nnfProperty xs = length xs < 30 ==> not(isInfixOf "-(" $ (show . nnf . arrowfree . head . parse . makeFormula) xs) 

checkNnf :: IO()
checkNnf = do
    quickCheck $ forAll genListOfParse $ nnfProperty


-- Since we know arrowfree and nnf works as expected and fulfill all pre- and postconditions of the CNF conversion, we can assume that this function will work as expected too 
toCNF :: Form ->  Form
toCNF f = (nnf . arrowfree) f

-- Result 3:
-- main checkArrofree
-- main checkNnf
-- main = toCNF <Form>

----------------------------------------------------------
-- 4) Time spent ~ 40 minutes (most of the work was done already in the previous exercises)

-- Functions reused from previous excercise: toCNF, arrowProperty, nnfProperty, parse, makeFormula, genListOfParse

-- A random formula generation function was implemented in excercise 2, I'll focus on a CNF specific implementation of it in this exercise

-- Property 1: a CNF version of a formula is equivalent to the original formula
cnfProperty1 :: [Int] -> Property
cnfProperty1 xs = length xs < 30 ==> equiv ((toCNF . head . parse . makeFormula) xs) ((head . parse . makeFormula) xs) 

-- Property 2: a CNF version of a formula can only negate atoms
cnfProperty2 :: [Int] -> Property
cnfProperty2 xs = length xs < 30 ==> not(isInfixOf "-(" $ (show . toCNF . head . parse . makeFormula) xs) 

-- Property 3: a CNF version of a formula can't contain arrows (entailment or equivalence)
cnfProperty3 :: [Int] -> Property
cnfProperty3 xs = length xs < 30 ==>  not(isInfixOf "=" $ (show . toCNF . head . parse . makeFormula) xs) 

checkCnf :: IO()
checkCnf = do
    quickCheck $ forAll genListOfParse $ cnfProperty1
    quickCheck $ forAll genListOfParse $ cnfProperty2
    quickCheck $ forAll genListOfParse $ cnfProperty3

-- main = checkCnf

main = print("Please check individual excercises for the results by uncommenting the appropriate lines of code")