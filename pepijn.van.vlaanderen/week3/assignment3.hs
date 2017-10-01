module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

{-
Exercise 1 (~1,5 hours)
-}
-- It is a contradiction when all the valuations are false
contradiction :: Form -> Bool
contradiction f = all (\x -> not (evl x f)) (allVals f)

-- It is a tautology when all valuations are true
tautology :: Form -> Bool
tautology f = all (\x -> evl x f) (allVals f)

-- | logical entailment
-- An entailment is an implication of f1 --> f2. Where when f1 is true, f2 is also true.
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

-- | logical equivalence
-- Both forms are equivalent (f1 == f2) and is a tautology.
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

-- Tests
testCon = contradiction (Cnj [p, Neg p])
testTau = tautology (Dsj [p, Neg p])
testEnt1 = entails p p
testEnt2 = entails p (Dsj [p, q])
testEqu1 = equiv p p
testEqu2 = equiv (Impl p q) (Impl (Neg q) (Neg p))

-- Test all tests
main1 = and [testCon, testTau, testEnt1, testEnt2, testEqu1, testEqu2]

{-
Exercise 2 (~30 minutes)
-}
-- There is a show instance in Lecture3 for Form, so this is fairly easy
-- Thus when you use show on a form and then parse it back, it should be equivalent to the form in a list (parse result is [Form])
testParse :: Form -> Bool
testParse f = parse (show f) == [f]

-- Test the testParse function with the form1, form2 & form3 from Lecture3 import
main2 = all testParse [form1, form2, form3]

{-
Exercise 3 (~1,5 hour)
-}
-- Arrowfree and negation normal form are given in Lecture 3.
-- Implement the step, distribute disjunctions inwards over conjunctions:
cnjOfDsj :: Form -> Form
cnjOfDsj (Prop x) = Prop x
cnjOfDsj (Neg f) = Neg f
cnjOfDsj (Cnj fs) = Cnj (map cnjOfDsj fs)
cnjOfDsj (Dsj fs)  = dist (map cnjOfDsj fs)

dist :: [Form] -> Form
dist [f] = f
dist (Cnj f1:fs) = Cnj (map (\f -> dist (f:fs)) f1)
dist (f1:Cnj f2:fs) = Cnj (map (\f -> dist (f:f1:fs)) f2)
dist (f1:fs) = Dsj (f1:[dist fs])

-- Simplify the conjunction and disjunction notations, so that disjunctions inwards over conjunctions
simplify :: Form -> Form
simplify (Cnj fs) = Cnj (simplifyCnj fs)
simplify (Dsj fs) = Dsj (simplifyDsj fs)
simplify f = f

simplifyCnj :: [Form] -> [Form]
simplifyCnj [] = []
simplifyCnj (Cnj fs:gs) = simplifyCnj (fs++gs)
simplifyCnj (f:fs) = simplify f : simplifyCnj fs

simplifyDsj :: [Form] -> [Form]
simplifyDsj [] = []
simplifyDsj (Dsj fs:gs) = simplifyDsj (fs++gs)
simplifyDsj (f:fs) = f : simplifyDsj fs

cnf = simplify . cnjOfDsj .  nnf . arrowfree

main3 = do
    print (Dsj [Cnj [p, q], Neg q, r])
    print (cnf (Dsj [Cnj [p, q], Neg q, r]))
