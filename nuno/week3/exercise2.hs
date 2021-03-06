module Lab3 where

import Data.List
import System.Random
import Lecture3

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
    n <- getRandomInt 20 -- Even number so the genFormN can take 2 at a time (the operator and the random literal)
    xs <- getIntL 5 n    -- 5 diferent operators therefore we want numbers between 0 and 4.
    let string = genFormN xs
    return string

-- Check if the  parser succeeded and returned something equal to what we had before
formEqualStr ::String -> Bool
formEqualStr s = let f = parse s in length f == 1 && s == show (head f)

testParse :: Int -> Int -> (String -> Bool) -> IO ()
testParse k n f = if k == n then print (show n ++ " tests passed")
                else do
                  form <- genForm
                  if (f form) then
                    testParse (k+1) n f
                  else error ("failed: " ++ show form)

testInvalid :: Bool
testInvalid = all (\x -> (parse x) == []) ["*+(1 2)", "(1 ==> 2", "*( +(1 2) +(1 3 )" , "(1 <=> <=> 2)", "-*-(1 2)", "(1 2 <=> 3)"]

main = do print "Testing with valid examples"
          testParse 0 100 formEqualStr
          print "Testing with invalid examples"
          print ("All tests passed: "++(show testInvalid))


-- time: 1h30