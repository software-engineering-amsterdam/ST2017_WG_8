module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

type Clause = [Int]
type Clauses = [Clause]

-- The part with allVals (Equiv f1 f2) is to get values for ALL names in x or in y
equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\v -> evl v f1 == evl v f2) (allVals (Equiv f1 f2))

-- Just some functions to simplify formulas ------------------

-- Cnj [(Cnj f), f2] is equivalent to Cnj [f,f2]
simplifyCnj :: [Form] -> [Form]
simplifyCnj [] = []
simplifyCnj ((Cnj f):fs) = f++(simplifyCnj fs)
simplifyCnj (f:fs) = (f:(simplifyCnj fs))

-- Dsj [(Dsj f), f2] is equivalent to Dsj [f,f2]
simplifyDsj :: [Form] -> [Form]
simplifyDsj [] = []
simplifyDsj ((Dsj f):fs) = f++(simplifyDsj fs)
simplifyDsj (f:fs) = (f:(simplifyDsj fs))

-- Simplify formulas. (P V (Q V R)) AND (P AND Q) should become (P V Q V R) AND P AND Q
simplify :: Form -> Form
simplify (Prop x) = Prop x
simplify (Neg f) = Neg f
simplify (Cnj fs) = Cnj (simplifyCnj (map simplify fs))
simplify (Dsj fs) = Dsj (simplifyDsj (map simplify fs))
--------------------------------------------------------------

-- Functions to generate random formulas ----------------
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (1,n))

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
           | n == 1 = "*("++(genFormN ns)++" "++(show n2)++")"  -- (phi AND P)
           | n == 2 = "+("++(genFormN ns)++" "++(show n2)++")"  -- (phi OR  P)
           | n == 3 = "-"++(genFormN ns)                        -- NOT phi
           | n == 4 = "("++(genFormN ns)++"<=>"++(show n2)++")" -- (phi <=> P)
           | n == 5 = "("++(genFormN ns)++"==>"++(show n2)++")" -- (phi ==> P)
           | otherwise = "-3 " --- Should never happen

genForm :: IO String
genForm = do
    n <- getRandomInt 12 -- Size of the formula. Bigger n --> Longer time to run tests
    xs <- getIntL 5 n    -- 5 diferent operators therefore we want numbers between 0 and 4.
    let string = genFormN xs
    return string

----------------------------------------------------------


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

-- It is much simpler to convert CNF's in simplified form (with no disjunctions inside disjunctions or conjuctions inside conjuctions)
cnf2cls :: Form -> Clauses
cnf2cls f = cnf2cls' (simplify f) where
    cnf2cls' (Prop x) = [[x]]    -- [[x]] is just a hack to keep it simple
    cnf2cls' (Neg (Prop x)) = [[-x]]
    cnf2cls' (Cnj fs) = [head (cnf2cls' x) | x <- fs]
    cnf2cls' (Dsj fs) = [[head ( head(cnf2cls' x)) | x <- fs]]

int2Lit :: Int -> Form
int2Lit x = if x < 0 then Neg (Prop (abs x)) else Prop x

cls2cnf :: Clauses -> Form
cls2cnf cl = Cnj (map (\x -> Dsj (map int2Lit x)) cl)

-- A form converted to cls and then to cnf back again is equivalent to the original form.
prop1 :: Form -> Bool
prop1 f = let fCNF = conv2cnf f in equiv fCNF (cls2cnf (cnf2cls fCNF))

-- A converted simplified form has the same structure as the original simplified form.
prop2 :: Form -> Bool
prop2 f = let simple = conv2cnf f in prop2' simple where
    prop2' (Cnj fs) = let conv = cnf2cls (Cnj fs) in length fs == length conv && (prop2'' fs conv) where
      prop2'' [] [] = True
      prop2'' ((Prop f):fs) (c:cs) = length c == 1 && (prop2'' fs cs)
      prop2'' ((Neg (Prop f)):fs) (c:cs) = length c == 1 && (prop2'' fs cs)
      prop2'' ((Dsj f):fs) (c:cs) = length f == length c && (prop2'' fs cs) -- Shouldn't have disjunctions here
    prop2' f = True


testProps :: Int -> Int -> (Form -> Bool) -> IO ()
testProps k n f = if k == n then print (show n ++ " tests passed")
                else do
                  form <- genForm
                  if (f (head (parse form))) then -- Assuming that parse function always parses things correctly and genForm generates good forms
                    testProps (k+1) n f
                  else error ("failed: " ++ show form)

-- We test for a converted form fCLS and the original form f if f and fCLS are equivalent and have the same structure.
main = do testProps 0 100 prop1
          testProps 0 100 prop2

-- time: 1 hour