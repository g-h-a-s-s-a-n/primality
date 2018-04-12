module Math.Arithmetic.Modular where

import Data.Maybe
import Math.Arithmetic.Modular.Types
  
-- | Modular inverse using the Extended Euclidean algorithm to find: 1/a = res mod m
-- In other words, we need to determine res such that: res * a = 1 mod n
-- Note that m and a should be coprimes
maybeModularInv :: Integral a => a -> a -> Maybe a
maybeModularInv m a
  | r' > 1 = Nothing
  | t' < 0 = Just $ t' + m
  | otherwise = Just t'
  where
    loop t r _ 0 = (t, r)
    loop t r newt newr = let q = r `div` newr
                         in  loop newt newr (t - q * newt) (r - q * newr)
    (t', r') = loop 0 m 1 a

-- | To perform modular division we use Modular inverse, which will help when performing modular arithmetic on integer, to transform a division into a multiplication
maybeModularDiv :: Integral a => a -> a -> a -> Maybe a
maybeModularDiv m a b = do
  k <- maybeModularInv m b
  return $ (a * k) `mod` m

modularDiv :: Integral a => a -> a -> a -> a
modularDiv m a b = fromMaybe (error "non-ivertible value!") $ maybeModularDiv m a b

-- | Modular square root of an Integer. Currently, this method is exponential, this method should be optimized using more efficient algorithms, especially if we consider that the modulo value is prime
modularSqrt :: Integral a => a -> a -> a
modularSqrt m a = head . filter (\k -> k * k `mod` m == a `mod` m) $ [0 .. m - 1]

-- | Quadratic residue when the modulo is prime
-- Ref: https://en.wikipedia.org/wiki/Tonelli%E2%80%93Shanks_algorithm
--modularPrimeSqrt :: Integral a => a -> a -> Maybe a
modularPrimeSqrt p n
  | eulerCriterion p n == 1 = Just $ loop s (z ^ q `mod` p) (n ^ q `mod` p) (n ^ ((q + 1) `div` 2) `mod` p)
  | otherwise = Nothing
  where
    factor2 n k -- n = q*(2^s)
      | even n = factor2 (n `div` 2) (k + 1)
      | otherwise = (n, k)
    (q, s) = factor2 (p - 1) 0
    -- eulerCriterion l k = k ^ ((l - 1) `div` 2) `mod` l
    eulerCriterion l k = mainVal $ (Mod k (Just l)) ^ ((l - 1) `div` 2)
    z = head . filter (\i -> eulerCriterion p i == p - 1) $ [2..p-1]
    loop _ _ 0 _ = 0
    loop _ _ 1 r = r
    loop m c t r = let i = sqrPow p t 1
                       b = sqrPow2 p c (m - i - 1)
                       bsqr = b ^ 2 `mod` p
                   in loop i bsqr (t * bsqr `mod` p) (r * b `mod` p)
    sqrPow m t i -- Find smallest i s.t. t^(2^i) = 1 (mod m)
      | res == 1 = i
      | otherwise = sqrPow m res (i + 1)
      where
        res = t ^ 2 `mod` m
    sqrPow2 m v 0 = v `mod` m -- v^(2^n) (mod m)
    sqrPow2 m v n = let res = sqrPow2 m v (n - 1)
                    in res ^ 2 `mod` m


-- | Jacobi Symbol (k|n)
jacobiSymbol :: Integral a => a -> a -> a
jacobiSymbol _ 1 = 1
jacobiSymbol n k
  | gcd n k /= 1 = 0
  | k > n = jacobiSymbol n (k `mod` n)
  | k `mod` 2 == 0 = j * jacobiSymbol n (k `div` 2)
  | otherwise = q * jacobiSymbol k n
    where
      q
        | k `mod` 4 == 3 && n `mod` 4 == 3 = -1
        | otherwise = 1
      j
        | n `mod` 8 == 3 || n `mod` 8 == 5 = -1
        | otherwise = 1
