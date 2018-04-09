module Math.NumberTheory.Primes.AKS where

import Math.Polynomial.Univariate
import Math.Polynomial.Univariate.Modular
import Data.Maybe
import Data.List

areCoprime a b = gcd a b == 1

coPrimes' [] _ = []
coPrimes' (l:ls) k
  | gcd k l == 1 = l:coPrimes' ls k
  | otherwise = coPrimes' (filter (\a -> (a `mod` l) > 0) ls) k

coPrimes n = coPrimes' [1 .. n] n

maybeMultOrder v m
  | not $ areCoprime v m  = Nothing
  | otherwise = fmap fst . listToMaybe . filter (\(k, x) -> x == 1 || x == 0) $ ls
  where
    ls = (\k -> (k, v^k `mod` m)) <$> [1 ..]

-- Instead of simply checking if the xth root of n is integer,
-- the number is rounded and elevated to the xth power to overcome
-- the approximation and quantization error of floats.
isPerfectPower n = any (\(b, a) -> round a ^ b == n) bases
  where
    maxBound = floor . logBase 2 . fromIntegral $ n
    bases = (\x -> (round x, fromIntegral n ** (1 / x))) <$> [2 .. fromIntegral maxBound]

smallestR' n = fst . head . filter (\(r, Just k) -> k > floor (logBase 2 (fromIntegral n) ^ 2)) . fmap (\r -> (r, maybeMultOrder n r)) $ [x | x<-[1..], gcd x n == 1]

smallestR n =  fst . head . filter (\(r, Just k) -> k > floor (logBase 2 (fromIntegral n) ^ 2)) . fmap (\r -> (r, maybeMultOrder n r)) . coPrimes' [1..] $ n

eulerTotient' n = genericLength [x | x <- [1..n], gcd n x == 1]

eulerTotient = genericLength . coPrimes

-- | Because 2 is added to the list manually in rdividen to drop all other even numbers checking. Because of this, 2 is not dropped from list, and should be tested apart
aksPrime 2 = True
aksPrime n = not (isPerfectPower n || rdividen) && (n <= r || not someAnotCongruent)
  where
    r = smallestR n
    maxA = floor $ (sqrt . fromIntegral . eulerTotient) r * (logBase 2 . fromIntegral) n
    rdividen = any (\k -> (n `mod` k) == 0) $ 2:[3,5 .. min r . floor . sqrt . fromIntegral $ n]
    qr1 = x ^ r - 1
    leftPoly k = PolyMod (x + fromInteger k) (Just qr1) (Just n)
    xn = PolyMod x (Just qr1) (Just n) ^ n
    rightPoly k = xn + fromInteger k
    someAnotCongruent = any (\a -> (leftPoly a) ^ n /= rightPoly a) [1 .. maxA]


aksPrime' 2 = True
aksPrime' n = not (isPerfectPower n || rdividen) && (n <= r || not someAnotCongruent)
  where
    r = smallestR' n
    maxA = floor $ (sqrt . fromIntegral . eulerTotient') r * (logBase 2 . fromIntegral) n
    rdividen = any (\k -> (n `mod` k) == 0) $ 2:[3,5 .. min r . floor . sqrt . fromIntegral $ n]
    qr1 = x ^ r - 1
    leftPoly k = generatePolyModPower qr1 n (fromIntegral k) n
    xn = generatePolyModPower qr1 n 0 n
    rightPoly k = xn + fromInteger k
    someAnotCongruent = any (\a -> leftPoly a /= rightPoly a) [1 .. maxA]
