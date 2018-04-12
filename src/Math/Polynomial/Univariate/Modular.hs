module Math.Polynomial.Univariate.Modular
  ( x
  , PolynomialMod(..)
  , generatePolyModPower
  , increaseByDeg
  , modularX
  , modularXpow
  ) where

import Data.List
import Math.Arithmetic.Modular
import Math.Polynomial.Univariate.LittleEndian

-- Little endian polynomial or general polynomials (check the one that corresponds to the chosen import)
type Poly i a = Polynomial a
-- type Poly i a = Polynomial i a

-- Usage of Nothing in divisorPoly and modVal results in using values
-- from the context (of other operations)
data PolynomialMod i a = PolyMod
  { mainPoly    :: Poly i a
  , divisorPoly :: Maybe (Poly i a)
  , modVal      :: Maybe i
  } deriving (Show, Eq) 

instance Functor (PolynomialMod i) where
  fmap f p = p{ mainPoly    =      f <$> mainPoly p
              , divisorPoly = fmap f <$> divisorPoly p}

instance (Integral i, Integral a) => Num (PolynomialMod i a) where
  p + p' = case matchModularity p p' of
    Nothing       -> error "Illegal PolynomialMod use: Mod does not match!"
    Just (cQ, cV) -> PolyMod (modPolyInt cQ (fromIntegral cV) $ mainPoly p + mainPoly p') (Just cQ) (Just cV)
    
  p * p' = case matchModularity p p' of
    Nothing       -> error "Illegal PolynomialMod use: Mod does not match!"
    Just (cQ, cV) -> PolyMod (modPolyInt cQ (fromIntegral cV) $ mainPoly p * mainPoly p') (Just cQ) (Just cV)

  signum = fmap signum
  
  fromInteger n = PolyMod (fromInteger n) Nothing Nothing
  
  negate = fmap negate
  
  abs = fmap abs -- The abs notion is not really well defined on polynomials

matchModularity :: (Num a, Eq a, Eq i) => PolynomialMod i a -> PolynomialMod i a -> Maybe (Poly b a, i)
matchModularity p p' = context
  where
    select f = case f p of
           Just q -> case f p' of
                       Just q' -> if q == q' then Just q else Nothing
                       Nothing -> Just q
           Nothing -> f p'
    context = do
      cQ <- select divisorPoly
      cV <- select modVal
      return (cQ, cV)

generatePolyPowMod :: Integral a => a -> a -> a -> Poly i a
generatePolyPowMod m a n = removeLeadingZerosPoly $ (`mod` m) <$> generatePolyPow a n

remPoly'  :: Integral a => a -> Poly i a -> Poly i a -> Poly i a
remPoly' m p = snd . polynomialDivision (modularDiv m) p

remPoly :: Integral a => Poly i a -> Poly i a -> Poly i a
remPoly p = snd . polynomialDivision (div) p

-- | p = res mod(q, m)
modPolyInt :: Integral a => Poly i a -> a -> Poly i a -> Poly i a
modPolyInt q m p  = removeLeadingZerosPoly $ (`mod` m) <$> remPoly p q

-- | (x+a)^n = res mod (q, m)
generatePolyModPower :: Integral b => Poly a b -> b -> b -> b -> PolynomialMod b b
generatePolyModPower q m a n = p * p' ^ (remainingDeg `div` (degQ - 1)) * p''
  where
    degQ = degPoly q
    divRes = n `div` (degQ - 1)
    p = PolyMod (generatePolyPowMod m a (n `mod` degQ)) (Just q) (Just m)
    remainingDeg = n - (n `mod` degQ)
    p' = PolyMod (generatePolyPowMod m a (min remainingDeg (degQ - 1))) (Just q) (Just m)
    p'' = PolyMod (generatePolyPowMod m a (remainingDeg `mod` (degQ - 1))) (Just q) (Just m)

modularX :: Integral a => a -> a -> PolynomialMod a a
modularX d m = PolyMod (Poly [0,1]) (Just $ x^d - 1) (Just m)

modularXpow ::  Integral a => a -> a -> a -> PolynomialMod a a
modularXpow d m k = (modularX d m) ^ k
