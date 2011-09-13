{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- 2.1
class Rat a where
  makeRat :: (Integral b, Integral c) => b -> c -> a
  numer :: (Integral b) => a -> b
  denum :: (Integral b) => a -> b

--instance (Rat a) => Show a where
--  show r = show (numer r) ++ "/" ++ show (denum r)

instance (Integral a) => Rat (a, a) where
  makeRat 0 _ = (0, 1)
  makeRat n_ d_ = (sgn * n `div` g, sgn * d `div` g)
    where
      n = fromIntegral n_
      d = fromIntegral d_
      sgn = signum d
      g = gcd n d
  numer = fromIntegral . fst
  denum = fromIntegral . snd

-- 2.2
data Point a = MakePoint a a deriving (Eq, Show)
xPoint :: Point a -> a
xPoint (MakePoint x _) = x
yPoint :: Point a -> a
yPoint (MakePoint _ y) = y

data Segment a = MakeSegment (Point a) (Point a) deriving (Eq, Show)
startSegment :: Segment a -> Point a
startSegment (MakeSegment a _) = a
endSegment :: Segment a -> Point a
endSegment (MakeSegment _ b) = b

midpointSegment :: (Fractional a) => Segment a -> Point a
midpointSegment (MakeSegment (MakePoint x1 y1) (MakePoint x2 y2)) =
  MakePoint ((x1 + x2) / 2) ((y1 + y2) / 2)

-- 2.3
segmentLength :: (RealFloat a) => Segment a -> a
segmentLength (MakeSegment (MakePoint x1 y1) (MakePoint x2 y2)) =
  sqrt ((x1 - x2)^2 + (y1 - y2)^2)

data Rectangle a = RectangleSides (Segment a) (Segment a)
                 | RectangleParallelSides (Segment a) (Segment a) deriving (Eq, Show)

perimeter :: (RealFloat a) => Rectangle a -> a
perimeter (RectangleSides a b) = 2 * (segmentLength a + segmentLength b)
perimeter (RectangleParallelSides a b) =
  let MakeSegment a1 _ = a
      MakeSegment b1 b2 = b
      c = MakeSegment a1 b1
      d = MakeSegment a1 b2
      in
        min (perimeter (RectangleSides a c)) (perimeter (RectangleSides a d))

area :: (RealFloat a) => Rectangle a -> a
area (RectangleSides a b) = segmentLength a * segmentLength b
area (RectangleParallelSides a b) =
  let MakeSegment a1 _ = a
      MakeSegment b1 b2 = b
      c = MakeSegment a1 b1
      d = MakeSegment a1 b2
      in
        min (area (RectangleSides a c)) (area (RectangleSides a d))

-- 2.4
-- Thanks to byorgey from #haskell for giving the Haskell-specific part
class Pair p where
  cons :: a -> b -> p a b
  car :: p a b -> a
  cdr :: p a b -> b

newtype ChurchPair a b = CP (forall c . (a -> b -> c) -> c)
instance Pair ChurchPair where
  cons x y = CP (\m -> m x y)
  car (CP z) = z (\p q -> p)
  cdr (CP z) = z (\p q -> q)

-- 2.5
consi :: Integer -> Integer -> Integer 
consi x y = 2^x * 3^y
cari, cdri :: Integer -> Integer
cari n | n `rem` 2 == 0 = 1 + cari (n `div` 2)
       | otherwise      = 0
cdri n | n `rem` 3 == 0 = 1 + cdri (n `div` 3)
       | otherwise      = 0

-- 2.6
zero :: a -> b -> b
zero = const id

{-
add1 :: a -> b -> c -> ?

a = b -> c -> d
b = d -> ?
a = (d -> ?) -> c -> d
=> a = (d -> e) -> c -> d
=> b = d -> e
=> c = c
=> add1 :: ((d -> e) -> c -> d) -> (d -> e) -> c -> e
-}

add1 :: ((d -> e) -> c -> d) -> (d -> e) -> c -> e
add1 n f x = f ((n f) x)

{-
one f x = add1 zero f x
        = f ((zero f) x)
        = f (id x)
        = f x
--}
one :: (a -> b) -> a -> b
one f x = f x
simple_one :: a -> a
simple_one = id

{-
two f x = add1 one f x
        = f ((one f) x)
        = f (f x)
--}
two :: (a -> a) -> a -> a
two f x = f $ f x

type ChurchNumeral a = (a -> a) -> a -> a
add :: (ChurchNumeral a) -> (ChurchNumeral a) -> (a -> a) -> a -> a
add n m f x = n f $ m f x

-- 2.7
makeInterval :: a -> a -> (a, a)
makeInterval = (,)
upperBound :: (a, a) -> a
upperBound = snd
lowerBound :: (a, a) -> a
lowerBound = fst

-- 2.8
subInterval :: (Num a) => (a, a) -> (a, a) -> (a, a)
subInterval a b = lower `makeInterval` upper
  where lower = lowerBound a - upperBound b
        upper = upperBound a - lowerBound b

-- 2.9 N.A.

-- 2.10
mulInterval :: (Real a) => (a, a) -> (a, a) -> (a, a)
mulInterval a b = let p1  = lowerBound a * lowerBound b
                      p2  = lowerBound a * upperBound b
                      p3  = upperBound a * lowerBound b
                      p4  = upperBound a * upperBound b
                      all = [p1, p2, p3, p4]
                  in foldl min p1 all `makeInterval` foldl max p1 all

divInterval :: (RealFrac a) => (a, a) -> (a, a) -> Maybe (a, a)
divInterval a (0, _) = Nothing
divInterval a (_, 0) = Nothing
divInterval a b      = Just (a `mulInterval` ((1 / upperBound b) `makeInterval` (1 / lowerBound b)))

-- 2.11
fastMulInterval :: (Real a) => (a, a) -> (a, a) -> (a, a)
fastMulInterval a b = uncurry makeInterval $ fmi (lowerBound a) (upperBound a) (lowerBound b) (upperBound b)
  where
    fmi x y z w | x >= 0 && z >= 0 = (x * z, y * w)
    
                | x >= 0 && w >= 0 = (y * z, y * w)
                | y >= 0 && z >= 0 = (x * w, y * w)

                | y >= 0 && w >= 0 = (min (x * w) (y * z), max (x * z) (y * w))

                | x >= 0           = (y * z, x * w)
                | z >= 0           = (x * w, y * z)

                | y >= 0           = (y * z, x * z)
                | w >= 0           = (w * z, x * z)

                | otherwise        = (y * w, x * z)

-- 2.12
makeCenterPercent :: (Real a) => a -> a -> (a, a)
makeCenterPercent x0 tol = let w = abs x0 * tol
                           in (x0 - w) `makeInterval` (x0 + w)

-- 2.13 N.A.
-- 2.14 N.A.
-- 2.15 N.A.
-- 2.16 N.A.

main = return ()
