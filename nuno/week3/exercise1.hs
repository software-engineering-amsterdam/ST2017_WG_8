module Lab3 where

import Lecture3

contradiction :: Form -> Bool
contradiction f = all (\ v -> not (evl v f)) (allVals f)

tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

-- A entails B if for all valuations, if A is true then B is also true
entails :: Form -> Form -> Bool
entails fA fB = all (\v -> if (evl v fA) then (evl v fB) else True) (allVals fA)

-- The part with allVals (Equiv f1 f2) is to get values for ALL names in x or in y
equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\v -> evl v f1 == evl v f2) (allVals (Equiv f1 f2))

-- Checking if definitions are correct with some well-known examples
checkContradiction = contradiction (Cnj [p, Neg p]) -- (P AND ~P) is a contradiction
checkTautology = tautology (Dsj [p, Neg p])         -- (P OR  ~P) is a tautology
checkEntails1 = entails p p                         -- P entails P
checkEntails2 = entails p (Dsj [p, q])              -- For all valuations where p is true, (P OR Q) is also true <=> p entails (P OR Q)
checkEquiv = equiv p p                              -- P is equivalent to P
checkEquiv2 = equiv (Dsj [p, q]) (Dsj [q, p])       -- (P OR Q) is equivalent to (Q OR P)

main = print (checkEquiv2)
-- Time: 25 minutes for now