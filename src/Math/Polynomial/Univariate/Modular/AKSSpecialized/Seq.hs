{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Math.Polynomial.Univariate.Modular.AKSSpecialized.Seq where

import Data.Sequence as S
import Data.Matchable

data PolynomialMod i a = PolyMod
                         { mainCoeffs :: Seq a
                         , divisorDeg :: Maybe i
                         , modVal     :: Maybe i
                         } deriving Show

shiftL, shiftR :: Seq a -> Seq a
shiftL Empty = Empty
shiftL l = rest :|> a
  where
    a :<| rest = l
    
shiftR Empty = Empty
shiftR l = a :<| rest
  where
    rest :|> a = l

mkProd :: Integral a => a -> Seq a -> Seq a -> Seq a -> Seq a
mkProd m r _ Empty = r
mkProd m r l (k :<| ks) = S.zipWith (addM m) r $ mkProd m (mulM m k <$> l) (shiftR l) ks

addM m a b = (a + b) `mod` m
mulM m a b = (a * b) `mod` m

instance (Eq a, Integral a) => Eq (PolynomialMod a a) where
  (==) = matchedOpFail f (==)
    where
      f (m, _) pp pp'= case dropWhileL id . S.zipWith (\k k' -> k `mod` m == k' `mod` m) pp $ pp' of
        Empty -> True
        _ -> False

instance Eq a => Matchable (PolynomialMod a a) where
  type Match (PolynomialMod a a) = (a, a)
  type ToUse (PolynomialMod a a) = Seq a
  toMatchValue p = do
    d <- divisorDeg p
    m <- modVal p
    return (d, m)
  toUseValue = mainCoeffs
  -- Redefine match for this instance, so that if one of
  -- the two values is present in one of each params, and the other
  -- is present in the other params, then combine them to be used
  -- together. The default implementation will work only if both are
  -- present
  match p p' = do
    d <- matched (divisorDeg p) (divisorDeg p')
    m <- matched (modVal p) (modVal p')
    return (d, m)
      where
        matched v v'
          | v == v'       = v
          | v  == Nothing = v'
          | v' == Nothing = v
          | otherwise = Nothing
        

instance (Integral a, Eq a) => Num (PolynomialMod a a) where
  (+) = matchedOpFail sumPoly (\_ _ -> error "Unmatched modulos")
    where
      sumPoly (d, m) k k' = PolyMod (S.zipWith (addM m) k k') (Just d) (Just m)
  (*) = matchedOpFail mulPoly (\_ _ -> error "Unmatched modulos")
    where
      mulPoly (d, m) k k' = PolyMod (mkProd m r k k') (Just d) (Just m)
        where
          r = S.replicate (fromIntegral d) 0
  negate p = p{mainCoeffs = negate <$> mainCoeffs p}
  fromInteger n = PolyMod (singleton . fromInteger $ n) Nothing Nothing
  abs = undefined
  signum p = p{mainCoeffs = signum <$> mainCoeffs p}

modularX :: Integral a => a -> a -> PolynomialMod a a
modularX d m = PolyMod (0 :<| 1 :<| S.replicate (fromIntegral d - 2) 0) (Just d) (Just m)

modularXpow :: Integral a => a -> a -> a -> PolynomialMod a a
modularXpow d m k = PolyMod (S.replicate (fromIntegral $ k `mod` d) 0 >< singleton 1 >< S.replicate (fromIntegral $ d - k `mod` d - 1) 0) (Just d) (Just m)
