{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Math.Polynomial.Univariate.LittleEndian where

import Data.Coerce
import Data.List

-- | Little endian polynomials
newtype Polynomial a = Poly
  { coeffs      :: [a]
  } deriving (Show, Functor) 

polyToList :: Polynomial a -> [a]
polyToList = coerce

listToPoly :: [a] -> Polynomial a
listToPoly = coerce

instance Num a => Num (Polynomial a) where
  p + p' = addPoly  p p'
  p * p' = multPoly p p'
  signum = fmap signum
  fromInteger n = Poly [fromInteger n]
  negate = fmap negate
  abs = fmap abs

instance (Num a, Eq a) => Eq (Polynomial a) where
  a == b = coeffs a' == coeffs b'
    where
      a' = removeLeadingZerosPoly a
      b' = removeLeadingZerosPoly b

degPoly :: Num b => Polynomial a -> b
degPoly p = genericLength (polyToList p) - 1

-- | Leading coefficient
leadingCoeff :: Num a => Polynomial a -> a 
leadingCoeff (Poly []) = 0
leadingCoeff p = last . polyToList $ p

-- | Least significant coefficient
constantCoeff :: Num a => Polynomial a -> a
constantCoeff (Poly []) = 0
constantCoeff p = head . polyToList $ p

nullPoly :: Num a => Polynomial a
nullPoly = Poly [0]

-- | Defining a variable that can be usefull to define polynomials
-- as in TeX notations. E.g. `y = x^2+4*x+5`
x :: Num a => Polynomial a
x = Poly [0, 1]

removeLeadingZerosPoly :: (Eq a, Num a) => Polynomial a -> Polynomial a
removeLeadingZerosPoly (Poly []) = nullPoly
removeLeadingZerosPoly p
  | leadingCoeff p == 0 = removeLeadingZerosPoly . listToPoly . init . polyToList $ p
  | otherwise = p

addPoly :: Num a => Polynomial a -> Polynomial a -> Polynomial a
addPoly p p' = listToPoly (opListWith (+) (coeffs p) (coeffs p'))

diffPoly :: Num a => Polynomial a -> Polynomial a -> Polynomial a
diffPoly p p' = listToPoly (opListWith (-) (coeffs p) (coeffs p'))

--negatePoly :: Num a => Polynomial a -> Polynomial a
-- signature necessary when using: negatePoly = fmap negate
--negatePoly p = fmap negate p

opListWith :: (t -> t -> t) -> [t] -> [t] -> [t]
opListWith _ l [] = l
opListWith _ [] l = l
opListWith op (l:ls) (l':ls') = (l `op` l'):opListWith op ls ls'

dropLeadingTerm :: Polynomial a -> Polynomial a
dropLeadingTerm p@(Poly []) = p
dropLeadingTerm p = listToPoly . init . polyToList $ p

dropConstTerm :: Polynomial a -> Polynomial a
dropConstTerm p@(Poly []) = p
dropConstTerm (Poly (_:ls)) = Poly ls

scalePoly :: Num a => a -> Polynomial a -> Polynomial a
scalePoly scalar = fmap (scalar *)

increaseDeg :: Num a => Polynomial a -> Polynomial a
increaseDeg = increaseByDeg 1

increaseByDeg :: Num a => Int -> Polynomial a -> Polynomial a
increaseByDeg n p = listToPoly $ replicate n 0 ++ polyToList p

polynomialDivision :: Num a => (a -> a -> a) -> Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
polynomialDivision op nm dnm
  | degDen <= degNum = (quotient, rest)
  | otherwise = (nullPoly, nm)
    where
      degDen = degPoly dnm
      degNum = degPoly nm
      degDiff = degNum - degDen
      qval = leadingCoeff nm `op` leadingCoeff dnm
      p = increaseByDeg degDiff $ Poly [qval]
      r = diffPoly (dropLeadingTerm nm) . increaseByDeg degDiff . scalePoly qval . dropLeadingTerm $ dnm
      (q, rest) = polynomialDivision op r dnm
      quotient = addPoly p q

evalPoly :: Num a => Polynomial a -> a -> a
evalPoly (Poly []) _ = 0
evalPoly p x = (constantCoeff p + x * evalPoly (dropConstTerm p) x)

multPoly :: Num a => Polynomial a -> Polynomial a -> Polynomial a
multPoly p p' = listToPoly (lstPr (coeffs p) (coeffs p'))
  where
    lstPr [] ls' = []
    lstPr (l:ls) ls' = opListWith (+) ((l *) <$> ls') (0:lstPr ls ls')

binom :: Integral a => a -> a -> a
binom n k = product [(n - k) + 1 .. n] `div` product [2 .. k]

allCoeffs :: Integral a => a -> [a]
allCoeffs p = binom p <$> [0 .. p]

-- | Using binomial coefficients to generate the polynomial efficiently: (x+a)^n 
generatePolyPow :: Integral a => a -> a -> Polynomial a
generatePolyPow a n = Poly cfs
  where
    cfs = zipWith (*) ((a^) . fromIntegral <$> [n,n-1..0]) (fromIntegral . binom n <$> [0..n])
