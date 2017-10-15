module Lab6 where

import System.Random
import System.CPUTime
import Text.Printf

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
-- ===================== Lab6 solutions =================
-------------------------------------------------------------

-- Get random number between 1 and n
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (1,n))


{-
Exercise 1 (~1,5 hours)
Also see Lectur6.hs exM definition, below a copy.
-}
exM :: Integer -> Integer -> Integer -> Integer
exM _ 0 _ = 1
exM x y n = let
                a = exM x (y `div` 2) n
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

-- Profiling of the code shows that the original function allocates aproximately 3,346,062,304 bytes in the heap when computing 
--  1000 integers in the range [1,1000000] with exponents in the range [1,1000000] and modules [1,1000000000]

-- The improved function is also more space efficient than the original function because it only allocates 14,063,008 bytes in the heap,
--  in the same situation as before. 

----------------------------------------

{-
Exercise 3 (~5 minutes)
-}

-- Composites are numbers that are greater than 1 and not primes 
-- (1 is not a composite because it only has one factor)
composites :: [Integer]
composites = filter (not.prime) [2..]

----------------------------------------

{-
Exercise 4 (~60 minutes)
-}

-- 1st argument is the primality check to be applied to each number of the list in the 3rd argument
testPrimes :: (Int -> Integer -> IO Bool) -> Int -> [Integer] -> IO ()
testPrimes primCheck k (x:xs) = do res <- primCheck k x
                                   if res then print ("Prime testing with k: " ++ show k ++ " failed on: " ++ show x)
                                   else testPrimes primCheck k xs

--If we increase k, primeTestsF will get more accurate results, 
--     therefore the greatest false positive will be a bigger number
main4 :: IO ()
main4 = do testPrimes primeTestsF 1  composites
           testPrimes primeTestsF 2  composites
           testPrimes primeTestsF 3  composites
           testPrimes primeTestsF 10 composites
           testPrimes primeTestsF 20 composites
           testPrimes primeTestsF 30 composites

-----------------------------------------

{-
Exercise 5 (~30 minutes)
-}

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

main5 :: IO ()
main5 = do testPrimes primeTestsF 1  carmichael
           testPrimes primeTestsF 2  carmichael
           testPrimes primeTestsF 3  carmichael
           testPrimes primeTestsF 10 carmichael
           testPrimes primeTestsF 20 carmichael
           testPrimes primeTestsF 30 carmichael
           print (take 5 carmichael)

------------------------------------------

{-
Exercise 6a (~15 minutes)
Exercise 6b (~45 minutes)
-}

-- It seems the Miller-Rabin primality check is not weak against Carmichael numbers.
--    Even for K == 1 the test returns 'Composite' for the first 5 Carmichael numbers the majority of times.

main6a = do testPrimes primeMR 1  carmichael
            testPrimes primeMR 2  carmichael
            testPrimes primeMR 3  carmichael
            -- For k > 3 it takes a lot of time to process
            print (take 5 carmichael)

largeMRPrime' :: [Integer] -> IO Int
largeMRPrime' (x:xs) = do isPrime <- primeMR 2 (2^x - 1)
                          if isPrime then return (fromInteger x)
                          else do p <- largeMRPrime' xs
                                  return p

-- MrPrime with exponent > n
largeMRPrime :: Int -> IO Int
largeMRPrime n = do p <- largeMRPrime' [x | x <- primes, x > toInteger n]
                    return p

--       Numbers found with this method (http://primes.utm.edu/mersenne/index.html?id=research&month=primes&day=mersenne&year=index#known) --
--10407932194664399081925240327364085538615262247266704805319112350403608059673360298012239441732324184842421613954281007791383566248323464908139906605677320762924129509389220345773183349661583550472959420547689811211693677147548478866962501384438260291732348885311160828538416585028255604666224831890918801847068222203140521026698435488732958028878050869736186900714720710555703168729087
-- Mersenne prime: yes (exponent = 1279)
--6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057151
-- Mersenne prime: yes (exponent = 521)
-- 531137992816767098689588206552468627329593117727031923199444138200403559860852242739162502265229285668889329486246501015346579337652707239409519978766587351943831270835393219031728127
-- Mersenne prime: yes (exponent = 607)
-- 446087557183758429571151706402101809886208632412859901111991219963404685792820473369112545269003989026153245931124316702395758705693679364790903497461147071065254193353938124978226307947312410798874869040070279328428810311754844108094878252494866760969586998128982645877596028979171536962503068429617331702184750324583009171832104916050157628886606372145501702225925125224076829605427173573964812995250569412480720738476855293681666712844831190877620606786663862190240118570736831901886479225810414714078935386562497968178729127629594924411960961386713946279899275006954917139758796061223803393537381034666494402951052059047968693255388647930440925104186817009640171764133172418132836351
-- Mersenne prime: yes (exponent = 2281)

main6b = do n <- getRandomInt 1000
            p <- largeMRPrime n
            print (2^p - 1)
            p2 <- largeMRPrime p
            print (2^p2 - 1)
            p3 <- largeMRPrime p2
            print (2^p3 - 1)

-------------------------------------------------------

{-
Exercise 7 (2~ hours)
-}

-- Generator with lower bound too
getRandomInt2 :: Int -> Int -> IO Int
getRandomInt2 n1 n2 = getStdRandom (randomR (n1,n2))


nextPrime :: Integer -> IO Integer
nextPrime n = do isPrime <- primeMR 2 n
                 if isPrime then return n 
                 else do x <- nextPrime (n+1)
                         return x

-- Generate number with 64*n bits
--      (based on generating n Ints (64 bits) and multiplying them)
--      (Actually the numbers are between 2^62 and 2^63 so they only have 63 bits.)
genBigNumber :: Int -> IO Integer
genBigNumber 1 = let range = maxBound :: Int in
                     do x <- getRandomInt2 (quot range 2) range
                        return (toInteger x)
genBigNumber n = let range = maxBound :: Int in
                     do x <- genBigNumber 1
                        y <- genBigNumber (n-1)
                        let n = x*y in return n

gen1024BitNumber :: IO Integer
gen1024BitNumber = genBigNumber (quot 1024 64)

rsaEncodeBlock :: (Integer,Integer) -> [Integer] -> [Integer]
rsaEncodeBlock pubKey bl = map (rsaEncode pubKey) bl

rsaDecodeBlock :: (Integer,Integer) -> [Integer] -> [Integer]
rsaDecodeBlock privKey bl = map (rsaDecode privKey) bl

-- Convert a possibly big integer into separate 64bit blocks
int2Blocks :: Integer -> [Integer]
int2Blocks 0 = []
int2Blocks n = [mod n (2^64)] ++ (int2Blocks (quot n (2^64)))

-- Convert several 64-bit blocks into a possibly big integer
blocks2Int :: [Integer] -> Integer
blocks2Int [] = 0
blocks2Int (x:xs) = x + (2^64)*(blocks2Int xs)

main7 =do n <- gen1024BitNumber
          p <- nextPrime n
          q <- nextPrime (p+1)
          let pubKey = rsaPublic  p q
              privKey = rsaPrivate p q
              plaintext = int2Blocks secret
          print "Secret"
          print secret
          print "P"
          print p
          print "Q"
          print q
          print "Public key"
          print pubKey
          print "Private key"
          print privKey
          print "Encrypting..."
          let enc =  rsaEncodeBlock pubKey plaintext
          print (blocks2Int enc)
          print "Decrypting..."
          print (blocks2Int (rsaDecodeBlock privKey enc))
          if (blocks2Int (rsaDecodeBlock privKey enc)) == secret 
            then print "All good" 
            else print "Something went wrong"

--time: 2 hours