{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Data.TypeLevel.PrimeProduct.SparseTest where

import Data.TypeLevel.Integer.Synonyms
import Data.TypeLevel.PrimeProduct.Sparse

-- TODO: Replace these tests with QuickCheck-style tests.

m0 = undefined :: Multiply            E             E             E  => x
m1 = undefined :: Multiply (P2:^:N1:::E)            E  (P2:^:N1:::E) => x
m2 = undefined :: Multiply (P2:^:P1:::E)            E  (P2:^:P1:::E) => x
m3 = undefined :: Multiply            E  (P2:^:N1:::E) (P2:^:N1:::E) => x
m4 = undefined :: Multiply            E  (P2:^:P1:::E) (P2:^:P1:::E) => x
m5 = undefined :: Multiply (P2:^:N1:::E) (P2:^:P1:::E)            E  => x
m6 = undefined :: Multiply (P2:^:P1:::E) (P2:^:N1:::E)            E  => x
m7 = undefined :: Multiply (P2:^:N1:::E) (P2:^:N1:::E) (P2:^:N2:::E) => x
m8 = undefined :: Multiply (P2:^:P1:::E) (P2:^:P1:::E) (P2:^:P2:::E) => x

d0 = undefined :: Divide            E             E             E  => x
d1 = undefined :: Divide (P2:^:N1:::E)            E  (P2:^:N1:::E) => x
d2 = undefined :: Divide (P2:^:P1:::E)            E  (P2:^:P1:::E) => x
d3 = undefined :: Divide            E  (P2:^:N1:::E) (P2:^:P1:::E) => x
d4 = undefined :: Divide            E  (P2:^:P1:::E) (P2:^:N1:::E) => x
d5 = undefined :: Divide (P2:^:N1:::E) (P2:^:P1:::E) (P2:^:N2:::E) => x
d6 = undefined :: Divide (P2:^:P1:::E) (P2:^:N1:::E) (P2:^:P2:::E) => x
d7 = undefined :: Divide (P2:^:N1:::E) (P2:^:N1:::E)            E  => x
d8 = undefined :: Divide (P2:^:P1:::E) (P2:^:P1:::E)            E  => x

l0 = undefined :: LCM            E             E             E  => x
l1 = undefined :: LCM (P2:^:N1:::E)            E             E  => x
l2 = undefined :: LCM (P2:^:P1:::E)            E  (P2:^:P1:::E) => x
l3 = undefined :: LCM            E  (P2:^:N1:::E)            E  => x
l4 = undefined :: LCM            E  (P2:^:P1:::E) (P2:^:P1:::E) => x
l5 = undefined :: LCM (P2:^:N1:::E) (P2:^:P1:::E) (P2:^:P1:::E) => x
l6 = undefined :: LCM (P2:^:P1:::E) (P2:^:N1:::E) (P2:^:P1:::E) => x
l7 = undefined :: LCM (P2:^:N1:::E) (P2:^:N1:::E) (P2:^:N1:::E) => x
l8 = undefined :: LCM (P2:^:P1:::E) (P2:^:P1:::E) (P2:^:P1:::E) => x

g0 = undefined :: GCD            E             E             E  => x
g1 = undefined :: GCD (P2:^:N1:::E)            E  (P2:^:N1:::E) => x
g2 = undefined :: GCD (P2:^:P1:::E)            E             E  => x
g3 = undefined :: GCD            E  (P2:^:N1:::E) (P2:^:N1:::E) => x
g4 = undefined :: GCD            E  (P2:^:P1:::E)            E  => x
g5 = undefined :: GCD (P2:^:N1:::E) (P2:^:P1:::E) (P2:^:N1:::E) => x
g6 = undefined :: GCD (P2:^:P1:::E) (P2:^:N1:::E) (P2:^:N1:::E) => x
g7 = undefined :: GCD (P2:^:N1:::E) (P2:^:N1:::E) (P2:^:N1:::E) => x
g8 = undefined :: GCD (P2:^:P1:::E) (P2:^:P1:::E) (P2:^:P1:::E) => x

