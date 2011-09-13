-- 1.1 N.A.

-- 1.2
ans1_2 = (/) ((+) 5
                  ((+) 4
                       ((-) 2
                            ((-) 3
                                 ((+) 6
                                      ((/) 4 5))))))
             ((*) 3
                  ((*) ((-) 6 2)
                       ((-) 2 7)))

-- 1.3
sosl2 :: (Num a, Ord a) => a -> a -> a -> a 
sosl2 a b c = if a < b
              then if c < a then sos a b else sos b c
              else if c < b then sos b a else sos a c
              where
                sos a b = a^2 + b^2

-- 1.4 N.A.
-- 1.5 N.A.

-- 1.7
squareRoot :: (Floating a, Ord a) => a -> a
squareRoot x = sqrtIter 1.0 0.0 x
  where
    sqrtIter :: (Floating a, Ord a) => a -> a -> a -> a
    sqrtIter guess prevGuess x =
      if goodEnough guess prevGuess
      then guess
      else sqrtIter (improve guess x) guess x
    improve guess x = average guess (x / guess)
    goodEnough guess prevGuess = abs (guess - prevGuess) / guess < 0.001
    average x y = (x + y) / 2

-- 1.8
cubeRoot :: (Floating a, Ord a) => a -> a
cubeRoot x = cubeIter 1.0 0.0 x
  where
    cubeIter :: (Floating a, Ord a) => a -> a -> a -> a
    cubeIter guess prevGuess x =
      if goodEnough guess prevGuess
      then guess
      else cubeIter (improve guess x) guess x
    improve guess x = (x / guess^2 + 2 * guess) / 3
    goodEnough guess prevGuess = abs (guess - prevGuess) / guess < 0.001

main = do
  return ()
