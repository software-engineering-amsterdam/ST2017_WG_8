--module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

type Clause = [Int]
type Clauses = [Clause]

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

-- It is much simpler to convert CNF's in simplified form (with no disjunctions inside disjunctions or conjuctions inside conjuctions)
cnf2cls :: Form -> Clauses
cnf2cls f = cnf2cls' (simplify f) where
    cnf2cls' (Prop x) = [[x]]    -- [[x]] is just a hack to keep it simple
    cnf2cls' (Neg (Prop x)) = [[-x]]
    cnf2cls' (Cnj fs) = [head (cnf2cls' x) | x <- fs]
    cnf2cls' (Dsj fs) = [[head ( head(cnf2cls' x)) | x <- fs]]

main = print (cnf2cls (Cnj [ Dsj [p, q], Dsj [q, r], r]))
-- time: 35 minutes for now
-- TODO: Test the function cnf2cls. Not sure how... Maybe test with individual examples only? Or implement a cls2cnf function?