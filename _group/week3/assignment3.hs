module Lab3 where
    
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

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

distribute :: [Form] -> Form
distribute [f1] = f1
-- Convert to a conjuction of disjuctions by distributing every sub-formula in the conjuction with the rest of the formulas in the outer disjunction.
distribute ((Cnj f1):fs) = Cnj (map (\f -> distribute (f:fs)) f1)
distribute (f1:(Cnj f2):fs) = Cnj (map (\f -> distribute (f:f1:fs)) f2)
distribute (f1:fs) = Dsj (f1:[(distribute fs)])

-- Convert formula to a conjunction of disjunctions. Distribute OR's to AND's if necessary
cnjOfDsj :: Form -> Form
cnjOfDsj (Prop x) = Prop x
cnjOfDsj (Neg f) = Neg f
cnjOfDsj (Cnj fs) = Cnj (map cnjOfDsj fs)
cnjOfDsj (Dsj fs)  = distribute (map cnjOfDsj fs) -- If a Disjunction is found, make sure any conjuctions inside it disappear

-- First remove arrows. Then transform to negation normal form. Then convert the form to a conjuction of disjunctions. Then simplify the result.
conv2cnf :: Form -> Form
conv2cnf f = simplify (cnjOfDsj (nnf (arrowfree f)))

main = do
    print  (Dsj [Cnj [p, q], Neg q, r])
    print (conv2cnf (Dsj [Cnj [p, q], Neg q, r]))

-- time: 1h30