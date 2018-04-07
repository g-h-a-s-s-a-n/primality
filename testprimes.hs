module Main where

import Primes
import System.Environment

main = do
  ns <- getArgs
  let answer = fmap (\(a)->(a, aksPrime . read $ a)) ns
  print answer
