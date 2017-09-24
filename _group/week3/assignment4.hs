module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

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

toCNF :: Form ->  Form
toCNF f = (nnf . arrowfree) f

equiv :: Form -> Form -> Bool
equiv fp fq = all (\ v ->  evl v fp == evl v fq) (allVals fp) 

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


main = checkCnf

-- time: 1 hour