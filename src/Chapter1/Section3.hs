import Debug.Trace (trace)

-- 1.29

cube :: (Num a) => a -> a
cube n = n^3

simpsonIntegral :: (Floating a) => Int -> (a -> a) -> a -> a -> a
simpsonIntegral n f a b = h / 3 * sum (map (\k -> coeff k * f (a + fromIntegral k * h)) [0..n])
  where
    h = (b - a) / fromIntegral n
    coeff k | k == 0 || k == n = 1
            | even k           = 2
            | otherwise        = 4

printCubeIntegral :: Int -> IO ()
printCubeIntegral n = putStrLn (show $ simpsonIntegral n cube 0 1)

-- 1.30
sumIter :: (Real a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
sumIter term a next b =
  let
    iter a result =
      if a > b
      then result
      else iter (next a) (result + term a)
    in
      iter a 0

-- 1.31
-- a.
myProduct :: (Real a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
myProduct term a next b = 
  if a > b
  then 1 
  else (term a) * myProduct term (next a) next b

factorial :: (Real a) => a -> a
factorial 0 = 1
factorial n = myProduct id 1 (\x -> x + 1) n

approximatePi :: Int -> Double
approximatePi precision = 4 * (myProduct term 1 (+1) precision)
  where
    term i = fromIntegral (2*i * 2*(i + 1)) / fromIntegral ((2*i + 1)^2)

-- b.
productIter :: (Real a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
productIter term a next b =
  let
    iter a result =
      if a > b
      then result
      else iter (next a) (result * term a)
    in
      iter a 1

-- 1.32
accumulate :: (Real a, Num b) => (b -> b -> b) -> b -> (a -> b) -> a -> (a -> a) -> a -> b
accumulate combiner nullValue term = g
  where
    g a next b =
      if a > b
      then nullValue
      else (term a) `combiner` g (next a) next b 

accuSum, accuProduct :: (Real a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
accuSum = accumulate (+) 0
accuProduct = accumulate (*) 1

accuIter :: (Real a, Num b) => (b -> b -> b) -> b -> (a -> b) -> a -> (a -> a) -> a -> b
accuIter combiner nullValue term a next b =
  let
    iter a result =
      if a > b
      then result
      else iter (next a) (result `combiner` term a)
    in
      iter a nullValue

-- 1.33
filteredAccumulate :: (Real a, Num b) => (a -> Bool) -> (b -> b -> b) -> b -> (a -> b) -> a -> (a -> a) -> a -> b
filteredAccumulate filter combiner nullValue term = g
  where
    g a next b | a > b     = nullValue
               | filter a  = (term a) `combiner` g (next a) next b
               | otherwise = g (next a) next b

-- copied from 1.22
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

-- a.
sosPrimes :: Integer -> Integer -> Integer
sosPrimes a b = filteredAccumulate isPrime (+) 0 (^2) a (+1) b

-- b.
prodRel :: Integer -> Integer
prodRel n = filteredAccumulate (\x -> x `gcd` n == 1) (*) 1 id 1 (+1) (n - 1)

-- 1.34 N.A.

-- 1.35
fixedPoint :: (RealFloat a) => (a -> a) -> a -> a
fixedPoint f = try
  where
    try guess | closeEnough guess next = next
              | otherwise              = try next
      where next = f guess
    closeEnough v1 v2 = abs (v1 - v2) < tolerance
    tolerance = 1e-5

approxPhi :: (RealFloat a) => a
approxPhi = fixedPoint (\x -> 1 + (1 / x)) 1

-- 1.36
verboseFixedPoint :: (RealFloat a) => (a -> a) -> a -> a
verboseFixedPoint f = try
  where
    try guess | trace (show guess) False = undefined
    try guess | closeEnough guess next = next
              | otherwise              = try next
      where next = f guess
    closeEnough v1 v2 = abs (v1 - v2) < tolerance
    tolerance = 1e-5

solutionOfxToxEqThousand = verboseFixedPoint (\x -> log(1000) / log(x)) 2

-- 1.37
-- a.
contFrac :: (Integral a, Fractional b) => (a -> a) -> (a -> a) -> a -> b
contFrac n d k = cf 1
  where
    cf x | x == k    = fromIntegral (n k) / fromIntegral (d k)  
         | otherwise = fromIntegral (n x) / (fromIntegral (d x) + cf (x + 1))

recipPhi :: (RealFloat a) => a
recipPhi = let f = const 1 in contFrac f f 10 

-- b.
contFracIter :: (Integral a, Fractional b) => (a -> a) -> (a -> a) -> a -> b
contFracIter n d k = cf (fromIntegral (n k) / fromIntegral (d k)) (k - 1)
  where
    cf acu 0 = acu
    cf acu k = cf (fromIntegral (n k) / (fromIntegral (d k) + acu)) (k - 1)

recipPhiIter :: (RealFloat a) => a
recipPhiIter = let f = const 1 in contFracIter f f 10 

-- 1.38
approxE :: (RealFloat a) => a
approxE = 2 + contFrac (const 1) d 10
  where
    d i | i `rem` 3 == 2 = 2 * (1 + i `div` 3)
        | otherwise      = 1

-- 1.39
contFracGeneral :: (Integral a, Fractional b) => (a -> b) -> (a -> b) -> a -> b
contFracGeneral n d k = cf 1
  where
    cf x | x == k    = n k / d k  
         | otherwise = n x / (d x + cf (x + 1))

tanCf :: (Integral a, RealFloat b) => a -> b -> b
tanCf k x = contFracGeneral n (\i -> 2*fromIntegral i - 1) k
  where
    n 1 = x
    n i = - x * x

approxTan :: (RealFloat a) => a -> a
approxTan = tanCf 100

-- 1.40
newtonsMethod :: (RealFloat a) => (a -> a) -> a -> a
newtonsMethod g guess = fixedPoint (newtonTransform g) guess
  where
    newtonTransform g x = x - ((g x) / ((deriv g) x))
    deriv g x = (g (x + dx) - g x) / dx
    dx = 1e-5

cubic :: (RealFloat a) => a -> a -> a -> a -> a
cubic a b c x = x^3 + a*x^2 + b*x + c

-- 1.41
double :: (a -> a) -> (a -> a)
double f = f . f

-- 1.42
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g x = f $ g x

-- 1.43
repeated :: (Integral c) => (a -> a) -> c -> a -> a
repeated f 0 = id
repeated f n = f . repeated f (n - 1)

-- 1.44
smooth :: (RealFloat a) => (a -> a) -> (a -> a)
smooth f x = (f (x - dx) + f x + f (x + dx)) / 3
  where dx = 1e-5

smoothN :: (RealFloat a, Integral b) => b -> (a -> a) -> (a -> a)
smoothN n f = (repeated smooth n) f

-- 1.45
averageDamp :: (RealFloat a) => (a -> a) -> a -> a
averageDamp f x = (f x + x) / 2

nthRoot :: (Integral a, RealFloat b) => a -> b -> b
nthRoot 0 x = 1
nthRoot n x = fixedPoint ((repeated averageDamp (times n)) (\y -> x / y^(n-1))) 1
  where
    times 1 = 0
    times n = 1 + times (n `div` 2)

-- 1.46
iterativeImprove :: (a -> Bool) -> (a -> a) -> a -> a
iterativeImprove goodEnough improve = ii where
  ii guess | goodEnough guess = guess
           | otherwise        = ii (improve guess)

sqrtIter :: (RealFloat a) => a -> a
sqrtIter x =
  iterativeImprove (\guess -> abs (guess^2 - x) < 1e-3) (\guess -> (guess + (x / guess)) / 2) 1

fixPoint :: (RealFloat a) => (a -> a) -> a -> a
fixPoint f guess = 
  iterativeImprove (\guess -> abs (f guess - guess) < tolerance) f guess
  where
    tolerance = 1e-5

main = return ()
