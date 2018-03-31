module Main where

import Data.List
import Data.Maybe

mygcd a b
  | a == 0 || b == 0 = a + b
  | otherwise = mygcd mn (mx `mod` mn)
  where
    mn = min a b
    mx = max a b
    
binom :: Integral a => a -> a -> a
binom n k = product [(n - k) + 1 .. n] `div` product [2 .. k]

recbinom :: Integral a => a -> a -> a
recbinom _ 0 =1
recbinom n k
  | n == k    = 1
  | otherwise = recbinom (n - 1) (k - 1) + recbinom (n - 1) k

coeff :: Integral a => a -> [a]
coeff p = fmap (binom p) [0 .. p]

modpCoeff :: Integral a => a -> [a]
modpCoeff p = fmap ((`mod` p) . binom p) [1 .. p `div` 2]

optCoeff p = fmap (`mod` p) . tail $ tempL 0 [1]
  where
    tempL k l
      | k >= p `div` 2 = l
      | otherwise = tempL (k + 1) (l ++ [(last l * (p - k)) `div` (k + 1)])
          

testPrime :: Integral a => a -> Bool
testPrime p = length (filter (== 0) (coeff p)) == fromIntegral (p - 1)

optimizedTestPrime p = genericLength (filter (== 0) (optCoeff p)) == (p `div` 2)
main = return ()

-- Find the smallest r s.t. n^k = 1 mod r for some k

multOrder v m = fst . head . filter (\(k, x) -> x == 1 || x == 0) $ ls
  where
    ls = (\k -> (k, v^k `mod` m)) <$> [1..]

maybeMultOrder v m
  | not $ areCoprime v m  = Nothing
  | otherwise = fmap fst . listToMaybe . dropWhile (\(k, x) -> not (x == 1 || x == 0)) $ ls
  where
    ls = (\k -> (k, v^k `mod` m)) <$> [1 ..]

areCoprime a b = gcd a b == 1

-- Instead of simply checking if the xth root of n is integer,
-- the number is rounded and elevated to the xth power to overcome
-- the approximation and quantization error of floats.
isPerfectPower n = any (\(b, a) -> round a ^ b == n) bases
  where
    maxBound = floor . logBase 2 . fromIntegral $ n
    bases = (\x -> (round x, fromIntegral n ** (1 / x))) <$> [2 .. fromIntegral maxBound]


smallestR n = fst . head . filter (\(r, Just k) -> k > floor (logBase 2 (fromIntegral n) ^ 2)) . fmap (\r -> (r, maybeMultOrder n r)) $ [x | x<-[1..], gcd x n == 1]

eulerTotient n = length [x | x <- [1..n], gcd n x == 1]

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

degPoly p = max 0 $ genericLength (coeffs p) + shiftFactor p - 1

reversePolyEndianness p = p{ coeffs=reverse (coeffs p)
                           , endianness = reverseEndianness . endianness $ p}

isLittleEndianPoly p = endianness p == Little
isBigEndianPoly p = endianness p == Big

-- | Most significant coefficient
msCoeff (Poly [] _ _) = 0
msCoeff (Poly ls Little _) = last ls
msCoeff (Poly ls _ _) = head ls

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

negatePoly = fmap negate

addList l [] = l
addList [] l = l
addList (l:ls) (l':ls') = (l + l'):addList ls ls'

diffPoly p = addPoly p . negatePoly

extractMSCoeff p@(Poly [] _ _) = p
extractMSCoeff p
  | isLittleEndianPoly p = p{coeffs = init . coeffs $ p}
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
      
