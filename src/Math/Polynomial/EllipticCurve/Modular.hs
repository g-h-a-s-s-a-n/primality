module Math.Polynomial.EllipticCurve.Modular
  ( module Math.Arithmetic.Modular.Types
  , module Math.Polynomial.EllipticCurve
  ) where

import Data.Matchable
import Math.Arithmetic.Modular
import Math.Arithmetic.Modular.Types
import Math.Polynomial.EllipticCurve

instance Integral a => ECOperatable (Modular a) where
  opSqrt (Mod v (Just m)) = Mod (modularSqrt m v) (Just m)
  opDiv p@(Mod v _) p'@(Mod v' _) = case (match p p') of
                 Nothing -> maybeMkMod (Just $ mainVal p `div` mainVal p') Nothing
                 Just m -> maybeMkMod (maybeModularDiv m v v') (Just m)
