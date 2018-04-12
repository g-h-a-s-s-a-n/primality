module Math.NumberTheory.Primes.EC where

import Data.Maybe
import Control.Monad
import Math.Arithmetic.Modular
import Math.Polynomial.EllipticCurve.Modular

ecppPrime :: Integral a => a -> Bool
ecppPrime n
  | n `mod` 2 == 0 = False
  | q == IdECPt = False
  | otherwise = True
  where
    x = head . dropWhile (\k -> jacobiSymbol n k /= -1) $ [1 .. n]
    y = head . dropWhile (\k -> jacobiSymbol n (x ^ 3 - k ^ 2) /= 1) $ [1 .. n]
    m = fromJust $ maybeModularDiv n (x ^ 3 - y ^ 2) x
    curve = EC (Mod (-m) (Just n)) 0
    q' = ECPt (Mod x (Just n)) (Mod y (Just n)) (Just curve)
    q = ecMult n q'

-- | Factorization using ECM
factorize :: Integral a => a -> [a]
factorize 1 = []
factorize n = case becameInvalid of
                [] -> [n]
                l:_ -> if g == 1 || g == n
                       then [n]
                       else factorize g ++ factorize (n `div` g)
                  where
                    g = gcd n . mainVal . rejectedDenum $ l
  where
    modN = Just n
    x0 = Mod 1 modN
    y0 = Mod 1 modN
    a k = (Mod k modN) ^ k
    b k = y0 ^ 2 - x0 ^ 3 - x0 * a k
    curve k = Just $ EC (a k) (b k)
    p k = ECPt x0 y0 (curve k)
    factorialsList = foldr (\i l -> i:((i *) <$> l)) [] [1..]
    maxVal = fst . head . dropWhile (\(_, k) -> k < n) . zip [1 ..] $ factorialsList
    ll k = foldl (\l i -> l ++ [ecMult i . last $ l]) [p k] [2 .. maxVal ^ 2]
    becameInvalid = dropWhile (not . isInvalidPt) . join $ ll <$> [1 .. maxVal]

ecmIsPrime n = (length . take 2 . factorize) n == 1
