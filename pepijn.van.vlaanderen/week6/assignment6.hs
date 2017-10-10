module Lab6 where

import           Data.List
import           Lecture6
import           System.CPUTime
import           System.Random
import           Text.Printf

{-
Exercise 1 (~1,5 hours)
Also see Lectur6.hs exM definition, below a copy.
-}
exM' :: Integer -> Integer -> Integer -> Integer
exM' _ 0 _ = 1
exM' x y n = let
                a = exM' x (y `div` 2) n
                b = (a*a) `rem` n
             in
                if even y then b
                else (x*b) `rem` n

{-
Exercise 2 (~30 minutes)
Use getCPUTime to measure the time difference between calculations.
-}
timeExM :: Integer -> Integer -> Integer -> IO ()
timeExM x y n = do
    start1 <- getCPUTime
    v1 <- expM x y n `seq` return ()
    end1 <- getCPUTime
    start2 <- getCPUTime
    v2 <- exM x y n `seq` return ()
    end2 <- getCPUTime
    let diff1 = fromIntegral (end1 - start1) / (10^12)
    let diff2 = fromIntegral (end2 - start2) / (10^12)
    printf "Time for expM %d %d %d: %.3f sec\n" x y n (diff1 :: Double)
    printf "Time for  exM %d %d %d: %.3f sec\n" x y n (diff2 :: Double)

-- When the exponent is increased, you see that the expM function is getting significantly slower.
main2 = do
    timeExM 12321 123456 12345
    timeExM 12321 1234567 12345
    timeExM 12321 12345678 12345

{-
Exercise 3 (~15 minutes)
Composite is not a prime and not unit 1, so start list from 2 and filter out primes.
Below the copy of composites in Lecture6.hs.
-}
composites' :: [Integer]
composites' = filter (not.prime) [2..]

{-
Exercise 4 (~1,5 hours)
Create a loop through a list, in our case the list of composites, and perform the primeTestsF function upon each element.
Print the false positive when found, otherwise continue the search.
-}
test4 :: Int -> [Integer] -> IO ()
test4 k [] = print ("Test4 with k=" ++ show k ++ ", no false positives")
test4 k (n:ns) =
    do
    result <- primeTestsF k n
    if result
        then
            print ("Test4 with k=" ++ show k ++ ", false positive on: " ++ show n)
        else
            test4 k ns

-- Test for k=1,2,3
-- The higher the value of k, the higher the first false positive value is.
main4 =
    do
    test4 1 composites
    test4 2 composites
    test4 3 composites
    test4 20 composites
    test4 30 composites
