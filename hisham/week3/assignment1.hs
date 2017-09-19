module Assignment1 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

--Assignment 1: Time: 5 hours
contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (\n -> evl n f) (allVals f)

entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)

test1 = Dsj [p, Neg q]
test2 = Cnj [p, Neg p]
test3 = Dsj [(p), (Neg p)]
test4 = Impl p (Dsj [p,q])
test5 = Impl (Dsj [p,q]) p

testA = Neg (Dsj[p,q])
testB = Cnj[p,q]
testC = Dsj[q, Neg q]
testD = Neg (Equiv p p)
testE = Impl (Neg p) (Neg q)

testAssignment1 :: IO()
testAssignment1 = do
    test test1
    test test2
    test test3
    test test4
    test test5

    testForms test3 test4
    testForms test5 test1
    testForms testA testB
    testForms testA testC
    testForms testA testD
    testForms testA testE
    testForms testB testA
    testForms testB testC
    testForms testB testD
    testForms testB testE
    testForms testC testA
    testForms testC testB
    testForms testC testD
    testForms testC testE
    testForms testD testA
    testForms testD testB
    testForms testD testC
    testForms testD testE
    testForms testE testA
    testForms testE testB
    testForms testE testC
    testForms testE testD


test :: Form -> IO ()
test f = do
    putStr (show f)
    putStr ": "
    if contradiction f then
        putStr "Contradiction "
    else
        putStr ""
    if satisfiable f then
        putStr "Satisfiable "
    else
        putStr ""
    if tautology f then
        putStr "Tautology"
    else
        putStr ""
    putStr "\n"

testForms :: Form -> Form -> IO ()
testForms f g = do 
    putStr (show f)
    putStr " "
    putStr (show g)
    putStr ": "
    if entails f g then
        putStr "Entailment "
    else
        putStr ""
    if equiv f g then
        putStr "Equivalent"
    else
        putStr ""
    putStr "\n"