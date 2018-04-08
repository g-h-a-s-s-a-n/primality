# primality

This package is an attempt to implement the [AKS primality testing algorithm](https://en.wikipedia.org/wiki/Aks_primality_test) using Haskell

## Building
First launch the command:

`$ cabal sandbox init`

Followed by:

`$ cabal install`

## Running
Test few primes using the AKS Primality test:

`$ cabal run 23 8191 15 173`

Resulting:

`[("23",True),("8191",True),("15",False),("173",True)]`
