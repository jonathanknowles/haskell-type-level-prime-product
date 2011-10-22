{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Data.TypeLevel.PrimeProduct.DenseTest where

import Data.TypeLevel.Integer
import Data.TypeLevel.Integer.Synonyms
import Data.TypeLevel.PrimeProduct.Dense

-- TODO: Replace these tests with QuickCheck-style tests.

m0 = undefined :: Multiply       E        E        E  => x
m1 = undefined :: Multiply (N1:::E)       E  (N1:::E) => x
m2 = undefined :: Multiply (P1:::E)       E  (P1:::E) => x
m3 = undefined :: Multiply       E  (N1:::E) (N1:::E) => x
m4 = undefined :: Multiply       E  (P1:::E) (P1:::E) => x
m5 = undefined :: Multiply (N1:::E) (P1:::E)       E  => x
m6 = undefined :: Multiply (P1:::E) (N1:::E)       E  => x
m7 = undefined :: Multiply (N1:::E) (N1:::E) (N2:::E) => x
m8 = undefined :: Multiply (P1:::E) (P1:::E) (P2:::E) => x

d0 = undefined :: Divide       E        E        E  => x
d1 = undefined :: Divide (N1:::E)       E  (N1:::E) => x
d2 = undefined :: Divide (P1:::E)       E  (P1:::E) => x
d3 = undefined :: Divide       E  (N1:::E) (P1:::E) => x
d4 = undefined :: Divide       E  (P1:::E) (N1:::E) => x
d5 = undefined :: Divide (N1:::E) (P1:::E) (N2:::E) => x
d6 = undefined :: Divide (P1:::E) (N1:::E) (P2:::E) => x
d7 = undefined :: Divide (N1:::E) (N1:::E)       E  => x
d8 = undefined :: Divide (P1:::E) (P1:::E)       E  => x

l0 = undefined :: LCM       E        E        E  => x
l1 = undefined :: LCM (N1:::E)       E        E  => x
l2 = undefined :: LCM (P1:::E)       E  (P1:::E) => x
l3 = undefined :: LCM       E  (N1:::E)       E  => x
l4 = undefined :: LCM       E  (P1:::E) (P1:::E) => x
l5 = undefined :: LCM (N1:::E) (P1:::E) (P1:::E) => x
l6 = undefined :: LCM (P1:::E) (N1:::E) (P1:::E) => x
l7 = undefined :: LCM (N1:::E) (N1:::E) (N1:::E) => x
l8 = undefined :: LCM (P1:::E) (P1:::E) (P1:::E) => x

g0 = undefined :: GCD       E        E        E  => x
g1 = undefined :: GCD (N1:::E)       E  (N1:::E) => x
g2 = undefined :: GCD (P1:::E)       E        E  => x
g3 = undefined :: GCD       E  (N1:::E) (N1:::E) => x
g4 = undefined :: GCD       E  (P1:::E)       E  => x
g5 = undefined :: GCD (N1:::E) (P1:::E) (N1:::E) => x
g6 = undefined :: GCD (P1:::E) (N1:::E) (N1:::E) => x
g7 = undefined :: GCD (N1:::E) (N1:::E) (N1:::E) => x
g8 = undefined :: GCD (P1:::E) (P1:::E) (P1:::E) => x
