module Math.NumberTheory.Primes.AKS where

import Math.Polynomial.Univariate.Modular.AKSSpecialized.Seq
import Data.Maybe
import Data.List

areCoprimes :: Integral a => a -> a -> Bool
areCoprimes a b = gcd a b == 1

coPrimes' :: Integral a => [a] -> a -> [a]
coPrimes' [] _ = []
coPrimes' (l:ls) k
  | gcd k l == 1 = l:coPrimes' ls k
  | otherwise = coPrimes' (filter (\a -> (a `mod` l) > 0) ls) k

coPrimes :: Integral a => a -> [a]
coPrimes n = coPrimes' [1 .. n] n

maybeMultOrder :: (Integral b, Integral a) => b -> b -> Maybe a
maybeMultOrder v m
  | not $ areCoprimes v m  = Nothing
  | otherwise = fmap fst . listToMaybe . filter (\(k, i) -> i == 1 || i == 0) $ ls
  where
    ls = (\k -> (k, v ^ k `mod` m)) <$> [1 ..]

-- Instead of simply checking if the xth root of n is integer,
-- the number is rounded and elevated to the xth power to overcome
-- the approximation and quantization error of floats.
isPerfectPower :: Integral a => a -> Bool
isPerfectPower n = any (\(b, a) -> round a ^ b == n) bases
  where
    maxBound = floor . logBase 2 . fromIntegral $ n
    bases = (\x -> (round x, fromIntegral n ** (1 / x))) <$> [2 .. fromIntegral maxBound]

smallestR' :: Integral a => a -> a
smallestR' n = fst . head . filter (\(r, Just k) -> k > floor (logBase 2 (fromIntegral n) ^ 2)) . fmap (\r -> (r, maybeMultOrder n r)) $ [i | i <- [1..], gcd i n == 1]

smallestR :: Integral a => a -> a
smallestR n =  fst . head . filter (\(r, Just k) -> k > floor (logBase 2 (fromIntegral n) ^ 2)) . fmap (\r -> (r, maybeMultOrder n r)) . coPrimes' [1..] $ n

eulerTotient :: Integral a => a -> a
eulerTotient n = genericLength [i | i <- [1..n], gcd n i == 1]

eulerTotient' :: Integral a => a -> a
eulerTotient' = genericLength . coPrimes

aksPrime :: Integral a => a -> Bool
aksPrime 2 = True
aksPrime n = not (isPerfectPower n || rdividen) && (n <= r || not someAnotCongruent)
  where
    r = smallestR' n
    maxA = floor $ (sqrt . fromIntegral . eulerTotient) r * (logBase 2 . fromIntegral) n
    rdividen = any (\k -> (n `mod` k) == 0) $ 2:[3,5 .. min r . floor . sqrt . fromIntegral $ n]
    leftPoly k = (modularX r n + fromIntegral k) ^ n
    rightPoly k = modularXpow r n n + fromIntegral k
    someAnotCongruent = any (\a -> leftPoly a /= rightPoly a) [1 .. maxA]

-- | Instead of using polynomials, we check that the polynomial comparison is valid when x = 0. No proof why this work, but tests shows that it is sufficient instead of testing the entire polynomial.
aksPrimeZero :: Integral a => a -> Bool
aksPrimeZero 2 = True
aksPrimeZero n = not (isPerfectPower n || rdividen) && (n <= r || not someAnotCongruent)
  where
    r = smallestR' n
    maxA = floor $ (sqrt . fromIntegral . eulerTotient) r * (logBase 2 . fromIntegral) n
    rdividen = any (\k -> (n `mod` k) == 0) $ 2:[3,5 .. min r . floor . sqrt . fromIntegral $ n]
    someAnotCongruent = any (\a -> powMod n a n /= a) [1 .. maxA]

-- | Compute a^k mod m
powMod :: (Integral a, Integral t) => a -> a -> t -> a
powMod m a k
  | k == 0 = 1
  | k `mod` 2 == 0 = (p * p) `mod` m
  | otherwise = (a * p * p) `mod` m
    where
      p = powMod m a (k `div` 2)

