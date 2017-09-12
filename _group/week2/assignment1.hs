module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

stdDeviation :: [Int] -> Float
stdDeviation (x1:x2:x3:x4:xs) = sqrt ((((fromIntegral x1)-2500)^2 + ((fromIntegral x2)-2500)^2 + ((fromIntegral x3)-2500)^2 + ((fromIntegral x4)-2500)^2) / 3)


-- Returns 0 if 0 <= x < 0.25, 1 if 0.25 <= x < 0.5, 2 if 0.5 <= x < 0.75 and 3 if 0.75 <= x < 1
    -- Red Curry is wrong in the way that his algorithm can also return the number 0, not just a number in the interval (0,1). Let's ignore that
classifier :: Float -> Int
classifier x | 0 <= x && x < 0.25 = 0
             | 0.25 <= x  && x < 0.5 = 1
             | 0.5 <= x  && x < 0.75 = 2
             | 0.75 <= x && x < 1 = 3
             | otherwise = -1

testRandomnessAux :: [Float] -> [Int] -> [Int]
testRandomnessAux [] ys = ys
testRandomnessAux (x:xs) (y1:y2:y3:y4:ys) = case (classifier x) of
                                               0 -> testRandomnessAux xs ((y1+1):y2:y3:y4:ys)
                                               1 -> testRandomnessAux xs (y1:(y2+1):y3:y4:ys)
                                               2 -> testRandomnessAux xs (y1:y2:(y3+1):y4:ys)
                                               3 -> testRandomnessAux xs (y1:y2:y3:(y4+1):ys)
                                               _ -> [0,0,0,0]

-- returns a list with 4 elements corresponding to the number of numbers in each quartile
testRandomness :: [Float] -> [Int]
testRandomness xs = testRandomnessAux xs [0,0,0,0]


-- assuming p-value = 0.05% of the mean = 10000/4 = 2500
pRange :: Float
pRange = 2500*0.05
postCond :: [Float] -> [Int] -> Bool
postCond xs ys = let p = ceiling pRange in sum ys == 10000 && stdDeviation ys <= pRange



testProbsAux :: Int -> Int -> ([Float] -> [Int])
                    -> ([Float] -> [Int] -> Bool) -> IO ()
testProbsAux k n f r = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- probs 10000
                  if r xs (f xs) then
                    do print ("pass:     quartiles: " ++ show (f xs) ++ " Standard Deviation: " ++ show (stdDeviation (f xs)))
                       testProbsAux (k+1) n f r
                  else error ("failed:    " ++ show (f xs) ++ " Standard Deviation: " ++ show (stdDeviation (f xs)))

testProbs = testProbsAux 0 100 testRandomness postCond
main = testProbs

--  Assuming p=0.05, this test is usually successful because the standard deviation of the distribution of the random numbers between the quartiles is most times lower than 2500*0.05=125
--  Of course, for lower p, this test will fail more often
--  time: 40 minutes