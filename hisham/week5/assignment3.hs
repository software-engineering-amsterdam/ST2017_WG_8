module Assignment3 where 

import Data.List
import System.Random
import Lecture5

-- Generates a random sudoku and a problem of the genarated sudoku
-- Get all filled positions and remove the filled positions one by one
-- Checks if the sudoku is still solvable by one solution
-- Else Non-unique solution has been found

testMinimal :: Int -> IO()
testMinimal 0 = print "All tests passed"
testMinimal n = do 
	rSudoku <- genRandomSudoku 
	s <- genProblem rSudoku 
	if any uniqueSol [eraseN s (r,c) | (r,c) <- filledPositions $ fst s] 
		then error "Non-unique solution found" 
	else testMinimal (n-1)

-- time: 3 hour