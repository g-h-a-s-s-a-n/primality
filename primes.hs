module Main where

import Data.List
import Data.Maybe
import Polynomial
import System.Environment

mygcd a b
  | a == 0 || b == 0 = a + b
  | otherwise = mygcd mn (mx `mod` mn)
  where
    mn = min a b
    mx = max a b
    
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

-- Find the smallest r s.t. n^k = 1 mod r for some k

multOrder v m = fst . head . filter (\(k, x) -> x == 1 || x == 0) $ ls
  where
    ls = (\k -> (k, v^k `mod` m)) <$> [1..]

maybeMultOrder v m
  | not $ areCoprime v m  = Nothing
  | otherwise = fmap fst . listToMaybe . filter (\(k, x) -> x == 1 || x == 0) $ ls
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

eulerTotient n = genericLength [x | x <- [1..n], gcd n x == 1]

-- | Because 2 is added to the list manually in rdividen to drop all other even numbers checking
aksPrime 2 = True
aksPrime n = not (isPerfectPower n || rdividen) && (n <= r || not someAnotCongruent)
  where
    r = smallestR n
    maxA = floor $ (sqrt . fromIntegral . eulerTotient) r * (logBase 2 . fromIntegral) n
    rdividen = any (\k -> (n `mod` k) == 0) $ 2:[3,5 .. min r . floor . sqrt . fromIntegral $ n]
    xr1 = Poly [1] Little r - 1
    powPl v k = normalizePoly $ generatePolyPowModular xr1 n v k
    someAnotCongruent = any (\a -> powPl a n /= (powPl 0 n + fromInteger a)) [1 .. maxA]

main = do
  n <- fmap read <$> getArgs
  let k = smallestR <$> n
  print k
  let e = eulerTotient <$> k
  print k
  let p = aksPrime <$> n
  print p
