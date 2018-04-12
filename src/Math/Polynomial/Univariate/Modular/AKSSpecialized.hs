module Math.Polynomial.Univariate.Modular.AKSSpecialized where

import Data.List
import Math.Arithmetic.Modular

-- | Little Endian Polynomials modulo (x^k -1, n)
data PolynomialMod i a = PolyMod
  { mainCoeffs :: [a]
  , divisorDeg :: Maybe i
  , modVal     :: Maybe i
  } deriving (Show)

instance (Integral i, Integral a) => Num (PolynomialMod i a) where
  p + p' = case matchModularity p p' of
             Nothing -> error "No Match!"
             Just (cD, cM) -> PolyMod (zipWith (\a a' -> (a + a') `mod` (fromIntegral cM)) (fixedLenCoeffs p) (fixedLenCoeffs p')) (Just cD) (Just cM) -- should not work with mainCoeffs, but with fixedLenCoeffs instead, yet it is working!
               where
                 fixedLenCoeffs = addTrailingZeros usedLen . mainCoeffs
                 usedLen = max cD $ min (genericLength . mainCoeffs $ p) (genericLength . mainCoeffs $ p')

  (*) = modularPolyMult
             
  negate p = fmap negate p
  signum p = fmap signum p
  abs p = undefined
  fromInteger n = PolyMod [fromInteger n] Nothing Nothing

instance (Integral i, Eq a, Num a) => Eq (PolynomialMod i a) where
  p@(PolyMod ls _ _) == p'@(PolyMod ls' _ _)
    = case matchModularity p p' of
        Nothing -> False
        Just (cD, _) -> addTrailingZeros cD ls == addTrailingZeros cD ls'

instance Functor (PolynomialMod i) where
  fmap f p = p{mainCoeffs = fmap f . mainCoeffs $ p}

addTrailingZeros :: (Integral i, Num a) => i -> [a] -> [a]
addTrailingZeros n l = genericTake n $ l ++ repeat 0

removeTrailingZeros :: (Eq a, Num a) => [a] -> [a]
removeTrailingZeros [] = []
removeTrailingZeros l
  | last l == 0 = removeTrailingZeros . init $ l
  | otherwise = l

matchModularity ::
  Eq i => PolynomialMod i a -> PolynomialMod i a -> Maybe (i, i)
matchModularity p p' = context
  where
    select f = case f p of
           Just q -> case f p' of
                       Just q' -> if q == q' then Just q else Nothing
                       Nothing -> Just q
           Nothing -> f p'
    context = do
      cD <- select divisorDeg
      cV <- select modVal
      return (cD, cV)

-- | Specialized modular multiplication to reduce memory size
-- | only works with divisors of the form (x^r-1)
modularPolyMult ::
  (Integral a, Integral i) =>
  PolynomialMod i a -> PolynomialMod i a -> PolynomialMod i a
modularPolyMult p p' = case matchModularity p p' of
  Nothing -> error "No Match while modularPolyMult"
  Just (cD, cV) -> PolyMod (fmap (`mod` (fromIntegral cV)) . mkProd (replicate (fromIntegral cD) 0) allPcoeffs . mainCoeffs $ p') (Just cD) (Just cV)
    where
      allPcoeffs = genericTake cD $ mainCoeffs p ++ repeat 0


-- | Direct computation of mkProd of two coefficients' lists, but inefficient since it uses shiftL which envolves reversing twice the list at each time. (Note could be implemented folding left or right)
origMkProd :: Num a => [a] -> [a] -> [a] -> [a]
origMkProd r l (k:ks) = mkProdb (zipWith (+) r (fmap (k *) l)) (shiftL l) ks

mkProdb :: Num a => [a] -> [a] -> [a] -> [a]
mkProdb r _ [] = r
mkProdb r l (k:ks) = mkProdb (zipWith (+) r (fmap (k *) l)) (shiftR l) ks

mkProd :: Num a => [a] -> [a] -> [a] -> [a]
mkProd r ls = reverse . mkProdb r (reverse ls)

mkProdc :: Num a => [a] -> [a] -> [a] -> [a]
mkProdc r _ [] = r
mkProdc r l (k:ks) = zipWith (+) r $ mkProdc (fmap (k *) l) (shiftR l) ks

mkProd''' :: Num a => [a] -> [a] -> [a] -> [a]
mkProd''' r l = reverse . fst . foldr f (r, reverse l) . reverse
  where
    f k (r', l') = (zipWith (+) r' ((k *) <$> l'), shiftR l')

mkProd'' :: (Foldable t, Num a) => [a] -> [a] -> t a -> [a]
mkProd'' r l = reverse . fst . foldl f (r, reverse l)
  where
    f (r', l') k = (zipWith (+) r' ((k *) <$> l'), shiftR l')
    
modularX ::  Integral a => a -> a -> PolynomialMod a a
modularX d m = PolyMod ([0,1]) (Just d) (Just m)

modularXpow ::  Integral a => a -> a -> a -> PolynomialMod a a
modularXpow d m k = PolyMod (genericReplicate (k `mod` d) 0 ++ [1]) (Just d) (Just m)

shiftR :: [a] -> [a]
shiftR [] = []
shiftR (l:ls) = ls ++ [l]

shiftL :: [a] -> [a]
shiftL = reverse . shiftR . reverse
{- shiftL l = let len = length l
           in take len . drop (len - 1) $ l ++ l
shiftL [] = []
shiftL ls = endElement:startList
  where (startList, endElement) = getInitLast ls

getInitLast [l] = ([], l)
getInitLast (l:ls) = (l:startList, endElement)
  where
    (startList, endElement) = getInitLast ls
-}
