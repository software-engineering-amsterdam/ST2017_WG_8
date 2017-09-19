module Lab3 where

import Data.List
import System.Random
import Lecture3

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
    n <- getRandomInt 20 -- Even number so the genFormN can take 2 at a time (the operator and the random literal)
    xs <- getIntL 4 n    -- 5 diferent operators therefore we want numbers between 0 and 4.
    let string = genFormN xs
    return string

-- Check if the  parser succeeded and returned something equal to what we had before
formEqualStr ::String -> [Form] -> Bool
formEqualStr s f = length f == 1 && s == show (head f)

testParse :: Int -> Int -> (String -> [Form]) -> (String -> [Form] -> Bool) -> IO ()
testParse k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  form <- genForm
                  if (r form (f form)) then
                    testParse (k+1) n f r
                  else error ("failed: " ++ show form)

main = testParse 0 100 parse formEqualStr

-- time: 1h20
-- TODO: Test with invalid examples too!