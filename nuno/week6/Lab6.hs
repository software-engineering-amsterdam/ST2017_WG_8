module Lab6 where

import System.Random

-- Solutions for the exercises at the bottom of this file

factorsNaive :: Integer -> [Integer]
factorsNaive n0 = factors' n0 2 where 
  factors' 1 _ = []
  factors' n m 
    | n `mod` m == 0 = m : factors' (n `div` m) m
    | otherwise      =     factors' n (m+1)

factors :: Integer -> [Integer]
factors n0 = let
   ps0 = takeWhile (\ m -> m^2 <= n0) primes
 in factors' n0 ps0 where 
   factors' 1 _  = []
   factors' n [] = [n]
   factors' n (p:ps) 
    | n `mod` p == 0 = p: factors' (n `div` p) (p:ps)
    | otherwise      =    factors' n ps

prime :: Integer -> Bool
prime n = factors n == [n]

primes :: [Integer]
primes = 2 : filter prime [3..]

mers :: Integer -> Integer
mers 1  = 2^2-1;    mers 2  = 2^3-1;     mers 3  = 2^5-1
mers 4  = 2^7-1;    mers 5  = 2^13-1;    mers 6  = 2^17-1
mers 7  = 2^19-1;   mers 8  = 2^31-1;    mers 9  = 2^61-1
mers 10 = 2^89-1;   mers 11 = 2^107-1;   mers 12 = 2^127-1
mers 13 = 2^521-1;  mers 14 = 2^607-1;   mers 15 = 2^1279-1
mers 16 = 2^2203-1; mers 17 = 2^2281-1;  mers 18 = 2^3217-1
mers 19 = 2^4253-1; mers 20 = 2^4423-1;  mers 21 = 2^9689-1
mers 22 = 2^9941-1; mers 23 = 2^11213-1; mers 24 = 2^19937-1
mers 25 = 2^21701-1;
mers _  = undefined

bin2int :: [Int] -> Int
bin2int = bin . reverse where
  bin []  = 0
  bin [0] = 0
  bin [1] = 1
  bin (0:bs) = 2 * bin bs
  bin (1:bs) = 2 * bin bs + 1
  bin _      = error "not a binary digit list"

addM :: Integer -> Integer -> Integer -> Integer
addM x y = rem (x+y)

multM :: Integer -> Integer -> Integer -> Integer
multM x y = rem (x*y) 

invM :: Integer -> Integer -> Integer
invM x n = let 
   (u,v) = fctGcd x n
   copr  = x*u + v*n == 1
   i     = if signum u == 1 then u else u + n  
 in 
   if copr then i else error "no inverse"

fGcd :: Integer -> Integer -> Integer
fGcd a b = if b == 0 then a
                     else fGcd b (rem a b)

fctGcd :: Integer -> Integer -> (Integer,Integer) 
fctGcd a b = 
  if b == 0 
  then (1,0) 
  else 
     let 
       (q,r) = quotRem a b
       (s,t) = fctGcd b r 
     in (t, s - q*t)

coprime :: Integer -> Integer -> Bool
coprime n m = fGcd n m == 1

coprime' :: Integer -> Integer -> Bool
coprime' n m = let (x,y) = fctGcd n m
               in x*n + y*m == 1

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

grow :: (node -> [node]) -> node -> Tree node

grow step seed = T seed (map (grow step) (step seed))

takeT :: Int -> Tree a -> Tree a

takeT 0 (T x _) = T x []
takeT n (T x ts) = T x (map (takeT (n-1)) ts)

coprimeT :: Tree (Integer,Integer)
coprimeT = grow f (1,1) 

f :: (Integer,Integer) -> [(Integer,Integer)]
f (n,m) = [(n+m,m),(n,n+m)]

pairs :: [(Integer,Integer)]
pairs = concatMap (\ n -> zip [1..n] (repeat n)) [1..]

coprimes :: [(Integer,Integer)]
coprimes = filter (uncurry coprime) pairs

expM ::  Integer -> Integer -> Integer -> Integer
expM x y = rem (x^y)

--exM :: Integer -> Integer -> Integer -> Integer
--

primeTestF :: Integer -> IO Bool
primeTestF n = do 
   a <- randomRIO (2, n-1) :: IO Integer
   return (exM a (n-1) n == 1)

primeTestsF :: Int -> Integer -> IO Bool
primeTestsF k n = do
 as <- sequence $ fmap (\_-> randomRIO (2,n-1)) [1..k]
 return (all (\ a -> exM a (n-1) n == 1) as)

decomp :: Integer -> (Integer,Integer)
decomp n0 = decomp' (0,n0) where
  decomp' = until (odd.snd) (\ (m,n) -> (m+1,div n 2))

mrComposite :: Integer -> Integer -> Bool
mrComposite x n = let
    (r,s) = decomp (n-1)
    fs     = takeWhile (/= 1) 
       (map (\ j -> exM x (2^j*s) n)  [0..r])
  in 
    exM x s n /= 1 && last fs /= (n-1)

primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = do 
    a <- randomRIO (2, n-1) :: IO Integer
    if exM a (n-1) n /= 1 || mrComposite a n
    then return False else primeMR (k-1) n

--composites :: [Integer]
--composites = error "not yet implemented"

encodeDH :: Integer -> Integer -> Integer -> Integer
encodeDH p k m = m*k `mod` p

decodeDH :: Integer -> Integer -> Integer -> Integer -> Integer
decodeDH p ga b c = let 
    gab' = exM ga ((p-1)-b) p 
  in 
    rem (c*gab') p

encode :: Integer -> Integer -> Integer -> Integer
encode p k m = let 
   p' = p-1
   e  = head [ x | x <- [k..], gcd x p' == 1 ]
 in 
   exM m e p

decode :: Integer -> Integer -> Integer -> Integer
decode p k m = let 
   p' = p-1
   e  = head [ x | x <- [k..], gcd x p' == 1 ]
   d  = invM e p' 
 in 
   exM m d p

cipher :: Integer -> Integer
cipher = encode secret bound

decipher :: Integer -> Integer
decipher = decode secret bound

totient :: Integer -> Integer
totient n = toInteger $ length [ k | k <- [1..n], gcd k n == 1 ]

phi :: Integer -> Integer -> Integer
phi p q = (p - 1) * (q - 1)

select :: Integer -> Integer -> Integer
select p q = let
   t = phi p q 
 in
   head [ x | x <- [3..], gcd x t == 1 ]

rsaPublic :: Integer -> Integer -> (Integer,Integer)
rsaPublic p q = let
    e = select p q
  in
    (e,p*q)

rsaPrivate ::  Integer -> Integer -> (Integer,Integer)
rsaPrivate p q = let 
   e = select p q
   t = phi p q 
   d = invM e t
  in 
   (d,p*q)

rsaEncode :: (Integer,Integer) -> Integer -> Integer 
rsaEncode (e,n) m =  exM m e n

rsaDecode :: (Integer,Integer) -> Integer -> Integer 
rsaDecode = rsaEncode                              

trapdoor :: (Integer,Integer) -> Integer -> Integer
trapdoor = rsaEncode 

secret, bound :: Integer                
secret = mers 18
bound  = 131
-------------------------------------------------------------

-- Exercise 1

exM :: Integer -> Integer -> Integer -> Integer
exM _ _ 1 = 0
exM x 0 n = 1
exM x y n = if mod y 2 == 0 then let res = exM x (quot y 2) n in mod (res*res) n else mod (x * (exM x (y-1) n)) n

----------------------------------------

-- Exercise 2

-- Get random number between 1 and n
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (1,n))

testExM :: (Integer -> Integer -> Integer -> Integer) -> Int -> IO ()
testExM f 0 = print "Done"
testExM f k = do  x <- getRandomInt 1000000
                  y <- getRandomInt 1000000
                  n <- getRandomInt 1000000000
                  print (f (toInteger x) (toInteger y) (toInteger n))
                  testExM f (k-1)

--main = testExM expM 1000
-- Measuring the time this takes to execute (with the unix command 'time') I got "user 0m48.612s".
--     This means the original modular exponentiation function toke 48.612 seconds to compute the exponentiation
--     of 1000 integers in the range [1,1000000] with exponents in the range [1,1000000] and modules in [1,1000000000]
-- Profiling of the code shows that this function allocates aproximately 3,346,062,304 bytes in the heap when executed.

--main = testExM exM 1000
-- Testing the new, hopefully more efficient function, I got the result "user 0m0.008s", meaning this function
--      is much more time efficient than the original.
-- In addition to being more time efficient, we can see that this function is also more efficient when it comes to memory usage,
--     because it only allocates around 14,063,008 bytes allocated in the heap when running (much less than the original).

----------------------------------------

-- Exercise 3

-- Composites are numbers that are greater than 1 and not primes 
-- (1 is not a composite because it only has one factor)
composites :: [Integer]
composites = filter (not.prime) [2..]

----------------------------------------

-- Exercise 4

testPrimes :: Int -> [Integer] -> IO ()
testPrimes k (x:xs) = do res <- primeTestsF k x
                         if res then print ("Prime testing with k: " ++ show k ++ " failed on: " ++ show x)
                         else testPrimes k xs

--If we increase k, primeTestsF will get more accurate results, 
--     therefore the greatest false positive will be a bigger number
--main :: IO ()
--main = do testPrimes 1  composites
--          testPrimes 2  composites
--          testPrimes 3  composites
--          testPrimes 10  composites
--          testPrimes 20 composites
--          testPrimes 30 composites

-----------------------------------------

-- Exercise 5

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

-- The Fermat's primality test checks if k^(n-1) mod n == 1 for random k's, 1 <= k < n
-- Accordingly to Wikipedia, all Carmichael numbers n pass this test for all k coprime with n
--    That means the chances for Fermat's primality test to give a false positive for a Carmichael number are high.
-- The first five Carmichael numbers given by the 'carmichael' function are [294409,56052361,118901521,172947529,216821881]
--     The tests above will probably output one of those numbers. We need k to be a big number in order to 
--     be minimally accurate for these numbers, but the greater the k, the more time it takes to check the primality of n.

main = do testPrimes 1  carmichael
          testPrimes 2  carmichael
          testPrimes 3  carmichael
          testPrimes 10 carmichael
          testPrimes 20 carmichael
          testPrimes 30 carmichael
          print (take 5 carmichael)

