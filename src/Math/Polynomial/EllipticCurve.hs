module Math.Polynomial.EllipticCurve where

import Data.Maybe

data EllipticCurve a = EC a a deriving (Eq, Show)

-- | The a value is for the x coordinates of the point.
data EllipticPoint a = PositiveECPt
                       { ecCoord :: a
                       , eCurve :: Maybe (EllipticCurve a)
                       } | NegativeECPt
                       { ecCoord :: a
                       , eCurve :: Maybe (EllipticCurve a)
                       } | IdECPt deriving (Eq, Show)

type EllipticPoint' a = Maybe (a, Bool)

getX :: EllipticPoint' a -> Maybe a
getX = fmap fst
getY (EC a b) pt = do
  (x, side) <- pt
  let ysqr = x ^ 3 + a * x + b
  if ysqr < 0
    then Nothing
    else if side
            then return $ sqrt ysqr
            else return $ -(sqrt ysqr)

negateECpt pt = do
  (x, side) <- pt
  return (x, not side)

addEC _ Nothing p = p
addEC _ p Nothing = p
addEC ec@(EC a b) p p' = do
  xp <- getX p
  xp' <- getX p'
  yp <- getY ec p
  yp' <- getY ec p'
  let s = if xp /= xp'
             then (yp - yp') / (xp - xp') -- Slope
             else if p == p'
                     then (3 * xp ^ 2 + a) / (2 * yp) -- Tangent slope
                     else error "Should not reach this point!"
      xr = s ^ 2 - xp - xp'
  if xp == xp' && p /= p'
    then Nothing
    else negateECpt . Just $ (xr, yp + s * (xr - xp) >= 0)

matchEC pt pt'
  | isNothing . eCurve $ pt  = eCurve pt'
  | isNothing . eCurve $ pt' = eCurve pt
  | otherwise = undefined

instance (Num a ) => Semigroup (EllipticPoint a) where
  (<>) = undefined

instance (Num a) => Monoid (EllipticPoint a) where
  mempty = IdECPt

instance Functor EllipticCurve where
  fmap f (EC a b) = EC (f a) (f b)

instance Functor EllipticPoint where
  fmap _ IdECPt = IdECPt
  fmap f pt = pt{ ecCoord =      f  . ecCoord $ pt
                , eCurve  = fmap f <$> eCurve   pt}
