module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Functions to generate random formulas ----------------
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   xs <- getIntL k (n-1)
   return (x:xs)

genFormN :: [Int] -> String
genFormN [] = "-3"
genFormN [n] = show n -- Needed to run
genFormN (n:n2:ns)
           | n == 0 = "*("++(genFormN ns)++" "++(show n2)++")"  -- (phi AND P)
           | n == 1 = "+("++(genFormN ns)++" "++(show n2)++")"  -- (phi OR  P)
           | n == 2 = "-"++(genFormN ns)                        -- NOT phi
           | n == 3 = "("++(genFormN ns)++"<=>"++(show n2)++")" -- (phi <=> P)
           | n == 4 = "("++(genFormN ns)++"==>"++(show n2)++")" -- (phi ==> P)
           | otherwise = "-3 " --- Should never happen

genForm :: IO String
genForm = do
    n <- getRandomInt 12 -- Size of the formula. Bigger n --> Longer time to run tests
    xs <- getIntL 4 n    -- 5 diferent operators therefore we want numbers between 0 and 4.
    let string = genFormN xs
    return string

----------------------------------------------------------

-- Just some functions to simplify formulas
simplifyCnj :: [Form] -> [Form]
simplifyCnj [] = []
simplifyCnj ((Cnj f):fs) = f++(simplifyCnj fs)
simplifyCnj (f:fs) = (f:(simplifyCnj fs))

simplifyDsj :: [Form] -> [Form]
simplifyDsj [] = []
simplifyDsj ((Dsj f):fs) = f++(simplifyDsj fs)
simplifyDsj (f:fs) = (f:(simplifyDsj fs))

-- Dsj [(Dsj f), f2] == Dsj [f,f2]. Same for Conjuctions.
simplify :: Form -> Form
simplify (Prop x) = Prop x
simplify (Neg f) = Neg f
simplify (Cnj fs) = Cnj (simplifyCnj (map simplify fs))
simplify (Dsj fs) = Dsj (simplifyDsj (map simplify fs))
--------------------------------------------------------------

-- Functions to convert to CNF -------------------------------
-- Distribute OR's to AND's if necessary
distribute :: [Form] -> Form
distribute [f1] = f1
distribute ((Cnj f1):fs) = Cnj (map (\f -> distribute (f:fs)) f1)
distribute (f1:(Cnj f2):fs) = Cnj (map (\f -> distribute (f:f1:fs)) f2)
distribute (f1:fs) = Dsj (f1:[(distribute fs)])

-- Convert formula to a conjunction of disjunctions
disOrAnd :: Form -> Form
disOrAnd (Prop x) = Prop x
disOrAnd (Neg f) = Neg f
disOrAnd (Cnj fs) = Cnj (map disOrAnd fs)
disOrAnd (Dsj fs)  = distribute (map disOrAnd fs) --disOrAnd (Cnj (map (Dsj (disOrAnd (Dsj gs))) fs))

conv2cnf :: Form -> Form
conv2cnf f = simplify (disOrAnd (nnf (arrowfree f)))

-------------------------------------------------------------

equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\v -> evl v f1 == evl v f2) (allVals (Equiv f1 f2))

inNNF :: Form -> Bool -- Is the formula in negation normal form?
inNNF (Prop x) = True
inNNF (Neg (Prop x)) = True
inNNF (Neg f) = False
inNNF (Cnj fs) = all inNNF fs
inNNF (Dsj fs) = all inNNF fs
inNNF (Impl f1 f2) = (inNNF f1) && (inNNF f2)
inNNF (Equiv f1 f2) = (inNNF f1) && (inNNF f2)

isArrowsFree :: Form -> Bool
isArrowsFree (Prop x) = True
isArrowsFree (Neg f) = isArrowsFree f
isArrowsFree (Cnj fs) = all isArrowsFree fs
isArrowsFree (Dsj fs) = all isArrowsFree fs
isArrowsFree (Impl _ _) = False
isArrowsFree (Equiv _ _) = False

isDsjOnly :: Form -> Bool
isDsjOnly (Prop x) = True
isDsjOnly (Neg f) = isDsjOnly f
isDsjOnly (Dsj fs) = all isDsjOnly fs
isDsjOnly f = False

isCnjOfDsj :: Form -> Bool
isCnjOfDsj (Prop x)  = True
isCnjOfDsj (Neg f) = isCnjOfDsj f
isCnjOfDsj (Cnj fs) = all isDsjOnly fs
isCnjOfDsj (Dsj fs) = all isDsjOnly fs
isCnjOfDsj f = False

-- Property that states that if we convert a formula to cnf we get a formula equivalent to the original
cnfEquiv :: Form -> Bool
cnfEquiv f = equiv f (conv2cnf f)

-- Property that states that if we convert a formula to cnf, only the literals may be negated.
cnfIsInNNF :: Form -> Bool
cnfIsInNNF f = inNNF (conv2cnf f)

-- Property that states that if we convert a formula to cnf the result won't have implications or equivalences
cnfIsArrowsFree :: Form -> Bool
cnfIsArrowsFree f = isArrowsFree (conv2cnf f)

-- Property that states that if we convert a formula to cnf the result will be a conjuction of disjunctions
cnfIsCnjOfDsj :: Form -> Bool
cnfIsCnjOfDsj f = isCnjOfDsj (conv2cnf f)

testProps :: Int -> Int -> (Form -> Bool) -> IO ()
testProps k n f = if k == n then print (show n ++ " tests passed")
                else do
                  form <- genForm
                  if (f (head (parse form))) then -- Assuming that parse function always parses things correctly and genForm generates good forms
                    testProps (k+1) n f
                  else error ("failed: " ++ show form)

main = do
    print "Testing if (to_cnf f) is equivalent to f"
    testProps 0 100 cnfEquiv
    print "Testing if (to_cnf f) is in nnf"
    testProps 0 100 cnfIsInNNF
    print "Testing if (to_cnf f) doesn't have any implications or equivalences"
    testProps 0 100 cnfIsArrowsFree
    print "Testing if (to_cnf f) is a conjuction of disjunctions"
    testProps 0 100 cnfIsCnjOfDsj 
    print "Test report: All tests passed!"

-- All tests passed
-- time: 1 hour