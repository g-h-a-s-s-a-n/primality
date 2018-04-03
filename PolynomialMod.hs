module PolynomialMod where

import Data.List
import Polynomial hiding (remPoly, modPolyInt)


-- | Little or Big endian polynoms
-- Usage of Nothing in quotPoly and modVal results in using values
-- from the context (of other operations)
data PolynomialMod i a = PolyMod
  { mainPoly    :: Polynomial i a
  , quotPoly    :: Maybe (Polynomial i a)
  , modVal      :: Maybe i
  } deriving (Show, Eq) 

instance Functor (PolynomialMod i) where
  fmap f p = p{mainPoly = fmap f $ mainPoly p, quotPoly = fmap (fmap f) $ quotPoly p}

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
  abs = fmap abs

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


generatePolyPowMod a n m = (`mod` m) <$> generatePolyPow a n

remPoly p = snd . polynomialDivision div p

-- | p = res mod(q, m)
modPolyInt q m p  = (`mod` m) <$> remPoly p q

-- | (x+a)^n = res mod (q, m)
generatePolyPowModular q m a n
  | n < degQ = p
  | otherwise = undefined
  where
    degQ = degPoly q
    p = generatePolyPowMod a degQ m
    p' = modPolyInt q m p
