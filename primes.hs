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

-- | Little or Big endian polynoms
data Poly a = LittlePoly [a] | BigPoly [a] deriving (Eq, Show) 

degPoly (LittlePoly ls) = max 0 $ length ls - 1
degPoly (BigPoly ls) = max 0 $ length ls - 1

reverseendianness (LittlePoly ls) = BigPoly . reverse $ ls
reverseEndianness (BigPoly ls) = LittlePoly . reverse $ ls

-- | Most significant coefficient
mscoeff (LittlePoly []) = 0
mscoeff (BigPoly []) = 0
mscoeff (LittlePoly ls) = last ls
mscoeff (BigPoly ls) = head ls

nullPoly = LittlePoly []

makeLittleEndian p@(LittlePoly ls) = p
makeLittleEndian p = reverseEndianness p

makeBigEndian p@(BigPoly ls) = p
makeBigEndian p = reverseEndianness p

addPoly p1 p2 = LittlePoly (addList lp1 lp2)
  where
    LittlePoly lp1 = makeLittleEndian p1
    LittlePoly lp2 = makeLittleEndian p2

negatePoly p = negate <$> p

addList l [] = l
addList [] l = l
addList (l:ls) (l':ls') = (l + l'):addList ls ls'

diffPoly p = addPoly p . negatePoly

extractMSCoeff p@(LittlePoly []) = p
extractMSCoeff p@(BigPoly []) = p
extractMSCoeff (LittlePoly ls) = LittlePoly (init ls)
extractMSCoeff (BigPoly ls) = BigPoly (tail ls)

scalePoly scalar = fmap (scalar *)

instance Functor Poly where
  fmap f (LittlePoly ls) = LittlePoly (fmap f ls)
  fmap f (BigPoly ls) = BigPoly (fmap f ls)

increaseDeg (LittlePoly ls) = LittlePoly (0:ls)
increaseDeg (BigPoly ls) = BigPoly (ls ++ [0])

increaseByDeg 0 p = p
increaseByDeg n p = increaseByDeg (n-1) (increaseDeg p)

polynomialDivision nm dnm
  | degDen <= degNum = (quotient, rest)
  | otherwise = (nullPoly, nm)
    where
      degDen = degPoly dnm
      degNum = degPoly nm
      degDiff = degNum - degDen
      qval = mscoeff nm / mscoeff dnm
      p = increaseByDeg degDiff $ LittlePoly [qval]
      r = diffPoly (extractMSCoeff nm) . increaseByDeg degDiff . scalePoly qval . extractMSCoeff $ dnm
      (q, rest) = polynomialDivision r dnm
      quotient = addPoly p q
      
