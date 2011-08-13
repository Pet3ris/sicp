import Control.Monad (when, mapM_)
import CPUTime (getCPUTime)
import System.Random (randomRIO)

-- 1.9 N.A.
-- 1.10 N.A.

-- 1.11
frec :: (Real a) => a -> a
frec n | n < 3     = n
       | otherwise = frec (n - 1) + 2 * frec (n - 2) + 3 * frec (n - 3)

fiter :: (Real a) => a -> a
fiter n | n < 3     = n
        | otherwise = fi 0 1 2 (n - 2)
        where
          fi _ _ c 0 = c
          fi a b c n = fi b c (c + 2 * b + 3 * a) (n - 1)

-- 1.12
comb :: (Integral a) => a -> a -> a
comb n k | k == 0 || k == n = 1
         | otherwise        = comb (n - 1) k + comb (n - 1) (k - 1)

-- 1.13 N.A.
-- 1.14 N.A.
-- 1.15 N.A.

-- 1.16
expIter :: (Real a, Integral b) => a -> b -> a
expIter b n = ei 1 n b
  where ei res n m | n == 0    = res
                   | even n    = ei res (n `div` 2) (m^2)
                   | otherwise = ei (res * m) (n `div` 2) (m^2)

-- 1.17
fastMult :: (Integral a) => a -> a -> a
fastMult a b | b < 0     = - fm (-b)
             | otherwise = fm b
               where 
                 fm 0 = 0
                 fm b | odd b     = a + fm (b - 1)
                      | otherwise = double (fm (halve b))
                 halve x  = x `div` 2
                 double x = x + x

-- 1.18
multIter :: (Integral a) => a -> a -> a
multIter a b | b < 0     = - mi 0 (-b) a
             | otherwise = mi 0 b a
               where
                 mi res b m | b == 0    = res
                            | even b    = mi res (halve b) (double m)
                            | otherwise = mi (res + m) (halve b) (double m)
                 halve x = x `div` 2
                 double x = x + x

-- 1.19
-- Tried to preserve the structure of the given lisp code.
fib :: (Integral a) => a -> a
fib n = fibIter 1 0 0 1 n
  where
    fibIter a b p q count | count == 0 = b
                          | even count =
                              fibIter a
                                      b 
                                      (p^2 + q^2)   -- p'
                                      (q^2 + 2*p*q) -- q'
                                      (div count 2)
                          | otherwise  =
                              fibIter (b*q + a*q + a*p)
                                      (b*p + a*q)
                                      p
                                      q
                                      (count - 1)

-- 1.20 N.A.
-- 1.21 N.A.

-- 1.22
-- Had to fix isPrime 1; one is not a prime
-- Added two more cases, because the given three are instantaneous with current processors
smallestDivisor :: (Integral a) => a -> a
smallestDivisor n = findDivisor n 2
  where
    findDivisor n testDivisor | testDivisor^2 > n       = n
                              | testDivisor `divides` n = testDivisor
                              | otherwise               = findDivisor n (testDivisor + 1)
    divides a b = b `rem` a == 0

isPrime :: (Integral a) => a -> Bool
isPrime 1 = False
isPrime n = smallestDivisor n == n

timedPrimeTest :: (Integral a) => (a -> Bool) -> a -> IO ()
timedPrimeTest isPrime n = do
  putStrLn ""
  putStr (show n)
  t <- getCPUTime
  startPrimeTest n t
  where
    startPrimeTest n startTime = do
      ip <- return $! (isPrime n)
      t <- getCPUTime
      when ip (reportPrime (t - startTime))
    reportPrime elapsedTime = putStrLn (" *** " ++ (show (fromIntegral elapsedTime / 1e12)))

searchForPrimes :: (Integral a) => [a] -> [a] 
searchForPrimes xs = filter isPrime xs

printFirstThree :: (Integral a) => (a -> Bool) -> [a] -> IO ()
printFirstThree isPrime xs = do
  mapM_ (timedPrimeTest isPrime) $ take 3 $ searchForPrimes xs

inputs :: (Integral a) => [[a]]
inputs = [[1001..], [10001..], [1000001..], [10^12+1..], [(10^13+1)..]]

findPrimes :: IO ()
findPrimes = mapM_ (printFirstThree isPrime) inputs

-- 1.23
findPrimesFast :: IO ()
findPrimesFast = mapM_ (printFirstThree isPrime) inputs
  where
    isPrime 1 = False
    isPrime n = smallestDivisor n == n
    smallestDivisor n = findDivisor n 2
    findDivisor n testDivisor | testDivisor^2 > n       = n
                              | testDivisor `divides` n = testDivisor
                              | otherwise               = findDivisor n (next testDivisor)
    next 2 = 3
    next n = n + 2
    divides a b = b `rem` a == 0

-- 1.24
expMod :: (Integral a, Integral b) => a -> b -> a -> a
expMod base exp m | exp == 0  = 1
                  | even exp  = (expMod base (exp `div` 2) m)^2 `rem` m
                  | otherwise = base * (expMod base (exp - 1) m) `rem` m

fermatTest :: (Integral a) => a -> a -> Bool
fermatTest n r = expMod r n n == r

fastPrime :: (Integral a) => [a] -> a -> Bool
fastPrime xs n = and (map (fermatTest n) xs)

getFermatIsPrime nTests = do
  randomNumbers <- sequence (replicate nTests (randomRIO (0 :: Integer, 1000)))
  return (fastPrime randomNumbers)

findPrimesFermat :: IO ()
findPrimesFermat = do
  isPrime <- getFermatIsPrime 100
  mapM_ (printFirstThree isPrime) inputs

-- 1.25 N.A.
-- 1.26 N.A.

-- 1.27
foolsTest :: (Integral a) => a -> Bool
foolsTest n = and (map (\a -> expMod a n n == a) [0..n-1])

carmichaelsFoolTest :: Bool
carmichaelsFoolTest = and (map foolsTest [561, 1105, 1729, 2465, 2821, 6601])

-- 1.28
expModMaybe :: (Integral a, Integral b) => a -> b -> a -> Maybe a
expModMaybe base exp m | exp == 0  = Just 1
                       | even exp  = do
                           toSquare <- (expModMaybe base (exp `div` 2) m)
                           squared <- return (toSquare^2 `rem` m)
                           if toSquare /= 1 || squared == 1 then Just squared else Nothing
                       | otherwise = do
                           value <- (expModMaybe base (exp - 1) m)
                           return (base * value `rem` m)

millerRabinTest :: (Integral a) => a -> a -> Bool
millerRabinTest n r = expModMaybe r (n - 1) n == Just 1

millerRabin :: (Integral a) => [a] -> a -> Bool
millerRabin xs n = and (map (millerRabinTest n) xs)

millerRabinIsPrime :: Int -> Integer -> IO Bool
millerRabinIsPrime _ 1 = return False
millerRabinIsPrime nTests n = do
  randomNumbers <- sequence (replicate nTests (randomRIO (1, n - 1)))
  return (millerRabin randomNumbers n)

checkPrimalities :: IO ()
checkPrimalities = do
  primalities <- mapM (millerRabinIsPrime 100) [1, 2, 3, 4, 5, 6, 11, 13, 20, 561, 1729, 6601, 1000033]
  mapM_ (putStrLn . show) primalities

main = return ()
