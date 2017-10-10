module Lab5 where

import Data.List
import System.Random
import Lecture5


allBlocks = [[(r,c) | r <- blocks!!(quot block 3), c <- blocks!!(mod block 3)] | block <- [0..8]]

combinationsOfDigits 0 _ _ = return []
combinationsOfDigits k n next = do
    x <- [next..10-k]
    xs <- combinationsOfDigits (k - 1) n (x+1)
    return (x : xs)

-- Combinations of n blocks
combinationsOfBlocks :: Int -> [[(Row, Column)]]
combinationsOfBlocks n = map (\x -> concat $ (map (\y -> allBlocks!!(y-1)) x)) (combinationsOfDigits n n 1)

-- Empty the selected blocks
removeBlocks :: Node -> [(Row, Column)] -> Node
removeBlocks n blks = removeBlocks' n blks where
  removeBlocks' n [] = n
  removeBlocks' n (x:xs) = removeBlocks' (eraseN n x) xs

-- Generate problem with n empty blocks by finding a random sudoku for which there is a permutation of n blocks
--        with the property that if you clear the numbers on those blocks, the sudoku problem has an unique solution.
genProblem' :: Int -> IO Node
genProblem' n = do s <- genRandomSudoku
                   let result = filter (\x -> uniqueSol x) [removeBlocks s x | x <- combinationsOfBlocks n] in 
                       if not (null result) then return (head result) else genProblem' n

main = do x <- genProblem' 4 -- Generate a sudoku with 4 empty blocks as an example
          showSudoku (fst x)

-- To check if a sudoku can have n empty blocks we could try to find an example of such a sudoku. Otherwise we have to 
--   run through all the possible sudoku's with n empty blocks and check if none of them have a unique solution or
--   we have to mathematically prove whether it is or not possible to find one sudoku with those properties.
-- Running the above program with n=5 doesn't return a solution even after a long time which doesn't really prove
--   anything but it suggests that 4 empty blocks is the maximum number of empty blocks we can have in a sudoku problem
--   with unique solution. After researching for a while I found that to be correct.
-- https://puzzling.stackexchange.com/questions/309/what-is-the-maximum-number-of-empty-3x3-blocks-a-proper-sudoku-can-have

-- time: 3 hours