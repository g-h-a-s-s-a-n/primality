module Math.Polynomial.Univariate where

import Data.List

-- | Endianness used with polynomials
data Endianness = Little | Big deriving (Eq, Show)

reverseEndianness Little = Big
reverseEndianness Big = Little

-- | Little or Big endian polynoms
data Polynomial i a = Poly
  { coeffs      :: [a]
  , endianness  :: Endianness
  , shiftFactor :: i
  } deriving (Show) 

instance Functor (Polynomial i) where
  fmap f p = p{coeffs = f <$> coeffs p}

instance (Integral i, Num a) => Num (Polynomial i a) where
  p + p' = addPoly  p p'
  p * p' = multPoly p p'
  signum = fmap signum
  fromInteger n = Poly [fromInteger n] Little 0
  negate = fmap negate
  abs = fmap abs

instance (Integral i, Num a, Eq a) => Eq (Polynomial i a) where
  a == b = coeffs a' == coeffs b' && shiftFactor a' == shiftFactor b'
    where
      a' = makeLittleEndian . normalizePoly $ a
      b' = makeLittleEndian . normalizePoly $ b

degPoly :: (Num i, Ord i) => Polynomial i a -> i
degPoly p = max 0 $ genericLength (coeffs p) + shiftFactor p - 1

reversePolyEndianness :: Polynomial i a -> Polynomial i a
reversePolyEndianness p = p{ coeffs=reverse (coeffs p)
                           , endianness = reverseEndianness . endianness $ p}

isLittleEndianPoly :: Polynomial i a -> Bool
isLittleEndianPoly p = endianness p == Little

isBigEndianPoly :: Polynomial i a -> Bool
isBigEndianPoly p = endianness p == Big

-- | Leading coefficient
leadingCoeff :: Num a => Polynomial i a -> a
leadingCoeff (Poly [] _ _) = 0
leadingCoeff (Poly ls Little _) = last ls
leadingCoeff (Poly ls _ _) = head ls

-- | Least significant coefficient
constantCoeff :: Num a => Polynomial i a -> a
constantCoeff (Poly [] _ _) = 0
constantCoeff (Poly ls Big _) = last ls
constantCoeff (Poly ls _ _) = head ls

nullPoly :: Num i => Polynomial i a
nullPoly = Poly [] Little 0

-- | Defining a variable that can be usefull to define polynomials
-- as in TeX notations. E.g. `y = x^2+4*x+5`
x :: (Num i, Num a) => Polynomial i a
x = Poly [1] Little 1

removeLeadingZerosPoly :: (Integral i, Eq a, Num a) => Polynomial i a-> Polynomial i a
removeLeadingZerosPoly p@(Poly [] _ _) = nullPoly
removeLeadingZerosPoly p@(Poly (0:ls) Big _) = removeLeadingZerosPoly p{coeffs = ls}
removeLeadingZerosPoly p
  | isLittleEndianPoly p && leadingCoeff p == 0 = removeLeadingZerosPoly p{coeffs = init . coeffs$ p}
  | otherwise = p
  
removeTrailingZerosPoly :: (Integral i, Eq a, Num a) => Polynomial i a-> Polynomial i a
removeTrailingZerosPoly p@(Poly [] _ _) = nullPoly
removeTrailingZerosPoly p@(Poly (0:ls) Little _) = removeTrailingZerosPoly p{coeffs = ls, shiftFactor = 1 + shiftFactor p}
removeTrailingZerosPoly p
  | isBigEndianPoly p && (last . coeffs) p == 0 = removeTrailingZerosPoly p{coeffs = init . coeffs $ p, shiftFactor = 1 + shiftFactor p}
  | otherwise = p

normalizePoly :: (Integral i, Eq a, Num a) => Polynomial i a -> Polynomial i a
normalizePoly = removeTrailingZerosPoly . removeLeadingZerosPoly

makeLittleEndian :: Polynomial i a -> Polynomial i a
makeLittleEndian p
  | isLittleEndianPoly p = p
  | otherwise = reversePolyEndianness p

makeBigEndian :: Polynomial i a -> Polynomial i a
makeBigEndian p
  | isBigEndianPoly p = p
  | otherwise = reversePolyEndianness p

unShiftPoly ::
  (Ord a1, Ord t, Num t, Num a1, Num a2) =>
  t -> Polynomial a1 a2 -> Polynomial a1 a2
unShiftPoly n p@(Poly _ _ s)
  | s < 1 || n < 1 = p
unShiftPoly n p = unShiftPoly (n - 1) unShiftedPoly
  where
    decShift = shiftFactor p - 1
    unShiftedPoly
      | isLittleEndianPoly p = p{coeffs = 0:coeffs p, shiftFactor = decShift}
      | otherwise = p{coeffs = coeffs p ++ [0], shiftFactor = decShift}

addPoly ::
  (Ord i, Num i, Num a) =>
  Polynomial i a -> Polynomial i a -> Polynomial i a
addPoly p p' = Poly (addList (coeffs lp) (coeffs lp')) Little minShift
  where
    minShift = min (shiftFactor p) (shiftFactor p')
    lp = makeLittleEndian . unShiftPoly (shiftFactor p - minShift) $ p
    lp' = makeLittleEndian . unShiftPoly (shiftFactor p' - minShift) $ p'

--negatePoly :: Num a => Polynomial a -> Polynomial a
-- signature necessary when using: negatePoly = fmap negate
--negatePoly p = fmap negate p

addList :: Num a => [a] -> [a] -> [a]
addList l [] = l
addList [] l = l
addList (l:ls) (l':ls') = (l + l'):addList ls ls'

diffPoly ::
  (Num a, Integral i) =>
  Polynomial i a -> Polynomial i a -> Polynomial i a
diffPoly p = addPoly p . negate

dropLeadingTerm :: Polynomial i a -> Polynomial i a
dropLeadingTerm p@(Poly [] _ _) = p
dropLeadingTerm p
  | isLittleEndianPoly p = p{coeffs = init . coeffs $ p}
  | otherwise = p{coeffs = tail . coeffs $ p}

dropConstantTerm :: Polynomial i a -> Polynomial i a
dropConstantTerm p@(Poly [] _ _) = p
dropConstantTerm p
  | isBigEndianPoly p = p{coeffs = init . coeffs $ p}
  | otherwise = p{coeffs = tail . coeffs $ p}

scalePoly :: (Functor f, Num b) => b -> f b -> f b
scalePoly scalar = fmap (scalar *)

increaseDeg :: Num i => Polynomial i a -> Polynomial i a
increaseDeg = increaseByDeg 1

increaseByDeg :: Num i => i -> Polynomial i a -> Polynomial i a
increaseByDeg n p = p{shiftFactor = n + shiftFactor p}

polynomialDivision :: (Integral i, Num a) => (a -> a -> a) -> Polynomial i a -> Polynomial i a -> (Polynomial i a, Polynomial i a)
polynomialDivision op nm dnm
  | degDen <= degNum = (quotient, rest)
  | otherwise = (nullPoly, nm)
    where
      degDen = degPoly dnm
      degNum = degPoly nm
      degDiff = degNum - degDen
      qval = leadingCoeff nm `op` leadingCoeff dnm
      p = increaseByDeg degDiff $ Poly [qval] Little 0
      r = diffPoly (dropLeadingTerm nm) . increaseByDeg degDiff . scalePoly qval . dropLeadingTerm $ dnm
      (q, rest) = polynomialDivision op r dnm
      quotient = addPoly p q

evalPoly :: (Integral i, Num p) => Polynomial i p -> p -> p
evalPoly (Poly [] _ _) _ = 0
evalPoly p x = (x ^ shiftFactor p) * (constantCoeff p + x * evalPoly (dropConstantTerm p{shiftFactor = 0}) x)

multPoly ::
  (Num i, Num a) =>
  Polynomial i a -> Polynomial i a -> Polynomial i a
multPoly p p' = res{shiftFactor = shiftFactor p + shiftFactor p'}
  where
    sp  = makeLittleEndian $ p {shiftFactor = 0}
    sp' = makeLittleEndian $ p'{shiftFactor = 0}
    lstPr [] ls' = []
    lstPr (l:ls) ls' = addList ((l *) <$> ls') (0:lstPr ls ls')
    res = Poly (lstPr (coeffs sp) (coeffs sp')) Little 0

binom :: Integral a => a -> a -> a
binom n k = product [(n - k) + 1 .. n] `div` product [2 .. k]

allCoeffs :: Integral a => a -> [a]
allCoeffs p = binom p <$> [0 .. p]

-- | Using binomial coefficients to generate the polynomial efficiently: (x+a)^n 
generatePolyPow :: (Num i, Integral a) => a -> a -> Polynomial i a
generatePolyPow a n = Poly cfs Little 0
  where
    cfs = zipWith (*) ((a^) <$> [n,n-1..0]) (binom n <$> [0..n])
