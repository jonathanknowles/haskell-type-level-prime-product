{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Type-level encodings and operations for products of prime numbers.
module Data.TypeLevel.PrimeProduct.Dense where

import Data.TypeLevel.Comparison
import Data.TypeLevel.Integer

infixr 0 :::

-- | A type-level representation of a /prime product/
--   encoded as a list of prime exponents [  /p/,   /q/,   /r/ ... ]
--   representing the prime product       [2^/p/, 3^/q/, 5^/r/ ... ].
data primeExponent ::: tail

-- | A type-level representation of the /empty/ prime product.
data E

-- | The result of evaluating type-level value /x/ at run-time.
data V x = V Integer

-- | Finds the greatest common divisor of product /x/ and product /y/.
class                                  GCD x y z | x y -> z
instance (G x y z', Normalise z' z) => GCD x y z

-- | Finds the least common multiple of product /x/ and product /y/.
class                              LCM      x       y       z | x y -> z
instance                           LCM      E       E       E
instance                           LCM      E  (q:::y) (q:::y)
instance                           LCM (p:::x)      E  (p:::x)
instance (LCM x y z, Max p q r) => LCM (p:::x) (q:::y) (r:::z)

-- | Divides product /x/ by product /y/ (assuming that /x/ is divisible by /y/).
class                                  Divide x y z | x y -> z
instance (D x y z', Normalise z' z) => Divide x y z

-- | Multiplies product /x/ with product /y/.
class                                   Multiply      x       y       z | x y -> z
instance                                Multiply      E       E       E
instance                                Multiply      E  (q:::y) (q:::y)
instance                                Multiply (p:::x)      E  (p:::x)
instance (Multiply x y z, Add p q r) => Multiply (p:::x) (q:::y) (r:::z)

-- | Normalises product /x/. Removes all trailing zero exponents.
class                                                  Normalise x y | x -> y
instance (Reverse x a, Decapitate a b, Reverse b y) => Normalise x y

-- | Iteratively removes zero exponents from the head of product /x/.
class                      Decapitate          x           y | x -> y
instance                   Decapitate          E           E
instance Decapitate x y => Decapitate (   Z :::x)          y
instance                   Decapitate ((S a):::x) ((S a):::x)

-- | Reverses the list that encodes product /x/.
class               Reverse x y | x -> y
instance R x E y => Reverse x y

-- | Divides product /x/ by product /y/ (assuming that /x/ is divisible by /y/),
--   producing a result that may require normalisation with 'Normalise'.
class                            D      x       y       z | x y -> z
instance                         D      E       E       E
instance (D E y E           ) => D      E  (Z:::y)      E
instance                         D (p:::x)      E  (p:::x)
instance (D x y z, Sub p q r) => D (p:::x) (q:::y) (r:::z)

-- | Finds the greatest common divisor of product /x/ and product /y/.
--   producing a result that may require normalisation with 'Normalise'.
class                            G      x       y       z | x y -> z
instance                         G      E       E       E
instance                         G      E  (q:::y)      E
instance                         G (p:::x)      E       E
instance (G x y z, Min p q r) => G (p:::x) (q:::y) (r:::z)

-- | Reverses the list that encodes product /x/, using the accumulator /a/.
class                     R      x  a y | x a -> y
instance                  R      E  a a
instance R x (p:::a) z => R (p:::x) a z

