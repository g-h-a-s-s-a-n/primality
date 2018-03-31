module Polynomial where

import Data.List

-- | Endianness used with polynomials
data Endianness = Little | Big deriving (Eq, Show)

reverseEndianness Little = Big
reverseEndianness Big = Little

-- | Little or Big endian polynoms
data Polynomial a = Poly
  { coeffs      :: [a]
  , endianness  :: Endianness
  , shiftFactor :: Integer
  } deriving (Eq, Show) 

instance Functor Polynomial where
  fmap f p = p{coeffs = f <$> coeffs p}

instance Num a => Num (Polynomial a) where
  p + p' = addPoly  p p'
  p * p' = multPoly p p'
  signum = fmap signum
  fromInteger n = Poly [fromInteger n] Little 0
  negate = fmap negate
  abs = fmap abs

degPoly p = max 0 $ genericLength (coeffs p) + shiftFactor p - 1

reversePolyEndianness p = p{ coeffs=reverse (coeffs p)
                           , endianness = reverseEndianness . endianness $ p}

isLittleEndianPoly p = endianness p == Little
isBigEndianPoly p = endianness p == Big

-- | Most significant coefficient
msCoeff (Poly [] _ _) = 0
msCoeff (Poly ls Little _) = last ls
msCoeff (Poly ls _ _) = head ls

-- | Least significant coefficient
lsCoeff (Poly [] _ _) = 0
lsCoeff (Poly ls Big _) = last ls
lsCoeff (Poly ls _ _) = head ls

nullPoly = Poly [] Little 0

removeSignificantZerosPoly p@(Poly [] _ _) = nullPoly
removeSignificantZerosPoly p@(Poly (0:ls) Big _) = p{coeffs = ls}
removeSignificantZerosPoly p
  | isLittleEndianPoly p && msCoeff p == 0 = removeSignificantZerosPoly p{coeffs = init . coeffs$ p}
  | otherwise = p

removeTrailingZerosPoly p@(Poly [] _ _) = nullPoly
removeTrailingZerosPoly p@(Poly (0:ls) Little _) = p{coeffs = ls, shiftFactor = 1 + shiftFactor p}
removeTrailingZerosPoly p
  | isBigEndianPoly p && (last . coeffs) p == 0 = removeTrailingZerosPoly p{coeffs = init . coeffs $ p, shiftFactor = 1 + shiftFactor p}
  | otherwise = p

normalizePoly = removeTrailingZerosPoly . removeSignificantZerosPoly

makeLittleEndian p
  | isLittleEndianPoly p = p
  | otherwise = reversePolyEndianness p

makeBigEndian p
  | isBigEndianPoly p = p
  | otherwise = reversePolyEndianness p

unShiftPoly n p@(Poly _ _ s)
  | s < 1 || n < 1 = p
unShiftPoly n p = unShiftPoly (n - 1) unShiftedPoly
  where
    decShift = shiftFactor p - 1
    unShiftedPoly
      | isLittleEndianPoly p = p{coeffs = 0:coeffs p, shiftFactor = decShift}
      | otherwise = p{coeffs = coeffs p ++ [0], shiftFactor = decShift}

addPoly p p' = Poly (addList (coeffs lp) (coeffs lp')) Little minShift
  where
    minShift = min (shiftFactor p) (shiftFactor p')
    lp = makeLittleEndian . unShiftPoly (shiftFactor p - minShift) $ p
    lp' = makeLittleEndian . unShiftPoly (shiftFactor p' - minShift) $ p'

--negatePoly :: Num a => Polynomial a -> Polynomial a
-- signature necessary when using: negatePoly = fmap negate
--negatePoly p = fmap negate p

addList l [] = l
addList [] l = l
addList (l:ls) (l':ls') = (l + l'):addList ls ls'

diffPoly p = addPoly p . negate

extractMSCoeff p@(Poly [] _ _) = p
extractMSCoeff p
  | isLittleEndianPoly p = p{coeffs = init . coeffs $ p}
  | otherwise = p{coeffs = tail . coeffs $ p}

extractLSCoeff p@(Poly [] _ _) = p
extractLSCoeff p
  | isBigEndianPoly p = p{coeffs = init . coeffs $ p}
  | otherwise = p{coeffs = tail . coeffs $ p}

scalePoly scalar = fmap (scalar *)

increaseDeg  p = p{shiftFactor = 1 + shiftFactor p}

increaseByDeg n p = p{shiftFactor = n + shiftFactor p}

polynomialDivision nm dnm
  | degDen <= degNum = (quotient, rest)
  | otherwise = (nullPoly, nm)
    where
      degDen = degPoly dnm
      degNum = degPoly nm
      degDiff = degNum - degDen
      qval = msCoeff nm / msCoeff dnm
      p = increaseByDeg degDiff $ Poly [qval] Little 0
      r = diffPoly (extractMSCoeff nm) . increaseByDeg degDiff . scalePoly qval . extractMSCoeff $ dnm
      (q, rest) = polynomialDivision r dnm
      quotient = addPoly p q

evalPoly (Poly [] _ _) _ = 0
evalPoly p x = (x ^ shiftFactor p) * (lsCoeff p + x * evalPoly (extractLSCoeff p{shiftFactor = 0}) x)

multPoly p p' = res{shiftFactor = shiftFactor p + shiftFactor p'}
  where
    sp  = makeLittleEndian $ p {shiftFactor = 0}
    sp' = makeLittleEndian $ p'{shiftFactor = 0}
    lstPr [] ls' = []
    lstPr (l:ls) ls' = addList ((l *) <$> ls') (0:lstPr ls ls')
    res = Poly (lstPr (coeffs sp) (coeffs sp')) Little 0