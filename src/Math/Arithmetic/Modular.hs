module Math.Arithmetic.Modular where

-- Naive approach
modularInv' m a = head . filter (\k -> k * a `mod` m == 1) $ [1 .. m-1]

-- | Modular inverse using the Extended Euclidean algorithm to find: 1/a = res mod m
-- In other words, we need to determine res such that: res * a = 1 mod n
-- Note that m and a should be coprimes
modularInv m a
  | r' > 1 = error $ "Not modularly invertible!"
  | t' < 0 = t' + m
  | otherwise = t'
  where
    loop t r _ 0 = (t, r)
    loop t r newt newr = let q = r `div` newr
                         in  loop newt newr (t - q * newt) (r - q * newr)
    (t', r') = loop 0 m 1 a

-- | To perform modular division we use Modular inverse, which will help when performing modular arithmetic on integer, to transform a division into a multiplication
modularDiv m a b = (a * modularInv m b) `mod` m
