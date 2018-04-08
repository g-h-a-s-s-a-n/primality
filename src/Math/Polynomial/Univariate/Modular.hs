module Math.Polynomial.Univariate.Modular where

import Data.List
import Math.Arithmetic.Modular
import Math.Polynomial.Univariate hiding (remPoly, modPolyInt)


-- | Little or Big endian polynoms
-- Usage of Nothing in quotPoly and modVal results in using values
-- from the context (of other operations)
data PolynomialMod i a = PolyMod
  { mainPoly    :: Polynomial i a
  , quotPoly    :: Maybe (Polynomial i a)
  , modVal      :: Maybe i
  } deriving (Show, Eq) 

instance Functor (PolynomialMod i) where
  fmap f p = p{ mainPoly =      f <$> mainPoly p
              , quotPoly = fmap f <$> quotPoly p}

instance (Integral i, Integral a) => Num (PolynomialMod i a) where
  p + p' = case matchModularity p p' of
    Nothing -> error "Illegal PolynomialMod use: Mod does not match!"
    Just (cQ, cV) -> p{mainPoly = modPolyInt cQ (fromIntegral cV) $ mainPoly p + mainPoly p'}
  p * p' = case matchModularity p p' of
    Nothing -> error "Illegal PolynomialMod use: Mod does not match!"
    Just (cQ, cV) -> p{mainPoly = modPolyInt cQ (fromIntegral cV) $ mainPoly p * mainPoly p'}
  signum = fmap signum
  fromInteger n = PolyMod (Poly [fromInteger n] Little 0) Nothing Nothing
  negate = fmap negate
  abs = fmap abs -- The abs notion is not really well defined on polynomials

matchModularity p p' = context
  where
    select f = case f p of
           Just q -> case f p' of
                       Just q' -> if q == q' then Just q else Nothing
                       Nothing -> Just q
           Nothing -> f p'
    context = do
      cQ <- select quotPoly
      cV <- select modVal
      return (cQ, cV)


generatePolyPowMod m a n = removeTrailingZerosPoly $ (`mod` m) <$> generatePolyPow a n

remPoly' m p = snd . polynomialDivision (modularDiv m) p
remPoly p = snd . polynomialDivision (div) p

-- | p = res mod(q, m)
modPolyInt q m p  = removeTrailingZerosPoly $ (`mod` m) <$> remPoly p q

-- | (x+a)^n = res mod (q, m)
generatePolyModPower q m a n = p * p' ^ (remainingDeg `div` (degQ - 1)) * p''
  where
    degQ = degPoly q
    divRes = n `div` (degQ - 1)
    p = PolyMod (generatePolyPowMod m a (n `mod` degQ)) (Just q) (Just m)
    remainingDeg = n - (n `mod` degQ)
    p' = PolyMod (generatePolyPowMod m a (min remainingDeg (degQ - 1))) (Just q) (Just m)
    p'' = PolyMod (generatePolyPowMod m a (remainingDeg `mod` (degQ - 1))) (Just q) (Just m)
            
