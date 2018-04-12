{-# LANGUAGE TypeFamilies #-}
module Math.Arithmetic.Modular.Types where

import Data.Matchable

data Modular a = Mod
                 { mainVal :: a
                 , modVal  :: Maybe a
                 } deriving Show

instance Eq a => Matchable (Modular a) where
  type Match (Modular a) = a
  type ToUse (Modular a) = a
  toMatchValue = modVal
  toUseValue = mainVal

mkMod :: Integral a => a -> Maybe a -> Modular a
mkMod v Nothing = Mod v Nothing
mkMod v (Just m) = Mod (v `mod` m) (Just m)

maybeMkMod :: Integral a => Maybe a -> Maybe a -> Maybe (Modular a)
maybeMkMod Nothing _ = Nothing
maybeMkMod (Just v) m = Just $ mkMod v m

instance Integral a => Eq (Modular a) where
  (==) = matchedOpFail (\m a a' -> a `mod` m == a' `mod` m) (==)

instance Integral a => Ord (Modular a) where
  (<=) = matchedOpFail (\m a a' -> f m a <= f m a') (<=)
    where
      f m k = if k < m `div` 2 then k else k - m

instance (Integral a) => Num (Modular a) where
  p@(Mod v m) + p'@(Mod v' m') = mkMod (v + v') $ match p p'
  p@(Mod v m) * p'@(Mod v' m') = mkMod (v * v') $ match p p'
  abs _ = undefined
  signum _ = undefined
  fromInteger n = Mod (fromIntegral n) Nothing
  negate (Mod v m) = mkMod (negate v) m
