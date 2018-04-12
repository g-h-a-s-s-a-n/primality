{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Data.Matchable where

import Data.Maybe

-- | This class is used to ensure that 2 variables of this class, share a common parameter prior to performing an operation. It is mainly used to avoid boilerplate code, when such common parameter is embedded inside the value instead of being called as an extra parameter to the function.
-- Note: This approach might not be the most efficient one. It should be more resource friendly to separate the value to match from the actual data (like when computing in a modulo ring). But the code would turn less protable.
class Eq (Match m) => Matchable m where
  type Match m
  type ToUse m
  toMatchValue :: m -> Maybe (Match m)
  toUseValue :: m -> ToUse m
  matchedOp :: (Match m -> ToUse m -> ToUse m -> a) -> m -> m -> Maybe a
  matchedOp op v v' = do
    m <- match v v'
    return $ op m (toUseValue v) (toUseValue v')
  matchedOpFail :: (Match m -> ToUse m -> ToUse m -> a) -> (ToUse m -> ToUse m -> a) -> m -> m -> a
  matchedOpFail op opFail v v' = case matchedOp op v v' of
    Nothing -> opFail (toUseValue v) (toUseValue v')
    Just m -> m
  match :: m -> m -> Maybe (Match m)
  match v v'
    | isNothing . toMatchValue $ v  = toMatchValue v'
    | isNothing . toMatchValue $ v' = toMatchValue v
    | toMatchValue v == toMatchValue v' = toMatchValue v
    | otherwise = Nothing

instance Matchable Integer where
  type Match Integer = Integer
  type ToUse Integer = Integer
  toUseValue = id
  toMatchValue = const Nothing
  matchedOp op v v' = Just $ op v v v'
