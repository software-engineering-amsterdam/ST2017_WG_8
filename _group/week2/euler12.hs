module Lab2 where

divisorsAux :: Int -> Int -> Int
divisorsAux k n | k*k == n = 2
                | k*k > n = 1
                | mod n k == 0 = 2+restOfThem
                | otherwise = restOfThem
                where restOfThem = divisorsAux (k+1) n

divisors :: Int -> Int
divisors n = divisorsAux 2 n

triangleNumAux :: Int -> Int -> [Int]
triangleNumAux start currSum = currSum : triangleNumAux (start+1) (currSum+start)

--triangleNum = triangleNumAux 2 1
triangleNum :: [Int]
triangleNum = [quot (x*(x+1)) 2 | x <- [1..]]

specialTriangleNum = [x | x <- triangleNum, divisors x >= 500]

main = print (head specialTriangleNum)