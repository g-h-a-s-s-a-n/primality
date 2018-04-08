module Main where

import Math.Number.Primes.AKS
import System.Environment

main = do
  ns <- getArgs
  let answer = fmap (\(a)->(a, aksPrime . read $ a)) ns
  print answer
