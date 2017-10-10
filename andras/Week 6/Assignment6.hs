import Lecture6

------------------------------------------------------------------
-- 1) Time spent: ~ 30 minutes


-- x^y mod n = result
--      x           y          n      result
exM' :: Integer -> Integer -> Integer -> Integer
exM' x 1 n = mod x n
exM' x y n = if mod y 2 == 0 then mod ((exM' x (y `div` 2) n) ^ 2) n  else mod ((exM' x (y `div` 2) n) ^ 2) n * mod x n


-- Result:
-- main = print(exM' 8 112 13)

-------------------------------------------------------------------
-- 2)


-------------------------------------------------------------------
-- 3) Time spent: ~ 20 minutes


composites' :: [Integer]
composites' = filter (not . prime) [2..]


-------------------------------------------------------------------
-- 4) Time spent: ~ 1 hour

testFermat :: Int -> [Integer] -> IO()
testFermat k (x:xs) = do
                        check <- primeTestsF k x 
                        if check then print(x)  else testFermat k xs


-- Result:
-- main = testFermat 1 composites
-- main = testFermat 2 composites
-- main = testFermat 3 composites



-------------------------------------------------------------------
-- 5)


-------------------------------------------------------------------
-- 6)

-------------------------------------------------------------------
-- 7)

main = print("")