{-# LANGUAGE TypeFamilies #-}
module Math.Polynomial.EllipticCurve where

import Data.Maybe
import Data.Matchable

data EllipticCurve a = EC a a deriving (Eq, Show)

-- | The a value is for the x coordinates of the point.
data EllipticPoint a = PositiveECPt
                       { ecCoordX :: a
                       , ecCurve  :: Maybe (EllipticCurve a)
                       } | NegativeECPt
                       { ecCoordX :: a
                       , ecCurve  :: Maybe (EllipticCurve a)
                       } | ECPt
                       { ecCoordX :: a
                       , ecCoordY :: a
                       , ecCurve  :: Maybe (EllipticCurve a)
                       } | IdECPt
                         | InvalidPt
                         -- ^ To complete the semigroup class,
                         -- otherwise, using maybe would be better.
                         -- It is encountered only when the modulo
                         -- value is not prime.
                       { rejectedDenum :: a
                       , ecCurve  :: Maybe (EllipticCurve a)
                       } deriving (Eq, Show)
  
instance Functor EllipticCurve where
  fmap f (EC a b) = EC (f a) (f b)

instance Functor EllipticPoint where
  fmap _ IdECPt = IdECPt
  fmap f (InvalidPt s c) = InvalidPt (f s) (fmap f <$> c)
  fmap f (ECPt ptX ptY c) = ECPt (f ptX) (f ptY) (fmap f <$> c)
  fmap f (PositiveECPt ptX c) = PositiveECPt (f ptX) (fmap f <$> c)
  fmap f (NegativeECPt ptX c) = NegativeECPt (f ptX) (fmap f <$> c)
  -- fmap f pt = pt{ ecCoordX =      f  . ecCoordX $ pt
  --              , ecCurve  = fmap f <$> ecCurve   pt}

instance ECOperatable a => Semigroup (EllipticPoint a) where
  (<>) = matchedOpFail addEC (const (const IdECPt))

instance ECOperatable a => Monoid (EllipticPoint a) where
  mempty = IdECPt

instance Eq a => Matchable (EllipticPoint a) where
  type Match (EllipticPoint a) = EllipticCurve a
  type ToUse (EllipticPoint a) = EllipticPoint a
  toMatchValue IdECPt = Nothing
  toMatchValue v = ecCurve v
  toUseValue v = v

isInvalidPt :: EllipticPoint a -> Bool
isInvalidPt (InvalidPt _ _) = True
isInvalidPt _ = False

addEC :: ECOperatable a
      => EllipticCurve a -> EllipticPoint a
      -> EllipticPoint a -> EllipticPoint a
addEC _ p@(InvalidPt _ _) _ = p
addEC _ _ p@(InvalidPt _ _) = p
addEC _ IdECPt p = p
addEC _ p IdECPt = p
addEC curve@(EC a _) p p'
  | xp /= xp' || p == p' = case slope of
      Nothing -> if xp /= xp'
                 then InvalidPt (xp - xp') (Just curve)
                 else InvalidPt (2 * yp) (Just curve)
      _ ->ECPt xres yres (Just curve)
  | otherwise = IdECPt
    where
      xp  = ecCoordX p
      xp' = ecCoordX p'
      yp  = getY p
      yp' = getY p'
      slope
        | xp /= xp' = (yp - yp') `opDiv` (xp - xp')
        | otherwise = (3 * xp ^ 2 + a) `opDiv` (2 * yp)
      realSlope = fromJust slope
      xres = realSlope ^ 2 - xp - xp'
      yres = -yp - realSlope * (xres - xp)

applySignECPt :: Num b => EllipticPoint a -> b -> b
applySignECPt (NegativeECPt _ _) = negate
applySignECPt _ = id

ecY :: ECOperatable a => EllipticCurve a -> a -> a
ecY curve xval = opSqrt . ysqr curve $ xval

ysqr :: ECOperatable a => EllipticCurve a -> a -> a
ysqr (EC a b) x = x ^ 3 + a * x + b 

getY :: ECOperatable a => EllipticPoint a -> a
getY (ECPt _ ptY _) = ptY
getY pt = applySignECPt pt . ecY (fromJust . ecCurve $ pt) . ecCoordX $ pt

negateECPt :: Num a => EllipticPoint a -> EllipticPoint a
negateECPt IdECPt = IdECPt
negateECPt p@(InvalidPt _ _) = p
negateECPt (PositiveECPt a b) = NegativeECPt a b
negateECPt (NegativeECPt a b) = PositiveECPt a b
negateECPt (ECPt a a' b) = ECPt a (-a') b

ecMult :: (Integral a, ECOperatable b) => a -> EllipticPoint b -> EllipticPoint b
ecMult _ p@(InvalidPt _ _) = p
ecMult _ IdECPt = IdECPt
ecMult 0 _ = IdECPt
ecMult 1 pt = pt
ecMult n pt
  | n < 0 = ecMult (-n) $ negateECPt pt
  | n `mod` 2 == 0  = halfWay <> halfWay
  | otherwise = halfWay <> pt <> halfWay
    where
      halfWay = ecMult (n `div` 2) pt

class (Ord a, Num a) => ECOperatable a where
  opSqrt :: a -> a
  opDiv  :: a -> a -> Maybe a

instance ECOperatable Float where
  opSqrt = sqrt
  opDiv _ 0 = Nothing
  opDiv a b = Just (a / b)

instance ECOperatable Double where
  opSqrt = sqrt
  opDiv _ 0 = Nothing
  opDiv a b = Just (a / b)
