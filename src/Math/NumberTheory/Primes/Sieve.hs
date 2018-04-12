module Math.NumberTheory.Primes.Sieve where

-- | Used to check the validity of the results returned by other algorithms
-- filter the multiple of start of a list. Helper function for the classical
-- sieve of Eratostenes primes finding.
sieve :: Integral a => [a] -> [a]
sieve [] = []
sieve (l:ls) = l:(filter (\k -> k `mod` l > 0) . sieve) ls

-- | List of all primes, using the sieve of Eratosthenes algorithm. Not efficient
-- But useful for testing.
sievePrimes :: Integral a => [a]
sievePrimes = sieve [2..]
