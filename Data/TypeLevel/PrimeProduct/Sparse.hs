{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Type-level encodings and operations for products of prime numbers.
module Data.TypeLevel.PrimeProduct.Sparse where

import Data.TypeLevel.Comparison
import Data.TypeLevel.Integer

infixr 0 :::
infixl 1 :^:

-- | A type-level representation of an /exponentiated prime number/ : a prime number raised to a positive integer power.
data prime :^: exponent

-- | A type-level representation of a /prime product/ : a list of exponentiated prime numbers in ascending order [2^/p/, 3^/q/, 5^/r/ ... ].
data exponentiatedPrime ::: tail

-- | A type-level representation of the /empty/ prime product.
data E

-- | The result of evaluating type-level value /x/ at run-time.
data V x = V Integer

-- | Converts product /x/ into a run-time value.
class                          Value      x  where value :: V x
instance                       Value (    Z) where value  = V (    0)
instance (Value t         ) => Value (P   t) where value  = V (x + 1) where V x = value :: V t
instance                       Value (    E) where value  = V (    1)
instance (Value a, Value p) => Value (a:^:p) where value  = V (a ^ p) where (V a, V p) = (value, value) :: (V a, V p)
instance (Value x, Value y) => Value (x:::y) where value  = V (x * y) where (V x, V y) = (value, value) :: (V x, V y)

-- | Normalises product /x/ by removing all factors with a zero exponent.
class                     Normalise                x                 y | x -> y
instance                  Normalise                E                 E
instance Normalise x y => Normalise (a:^:(P n) ::: x) (a:^:(P n) ::: y)
instance Normalise x y => Normalise (a:^:   Z  ::: x)                y

-- | Multiplies product /x/ with product /y/.
class                                             Multiply x y z | x y -> z
instance (C x y c, M c x y z', Normalise z' z) => Multiply x y z

-- | Divides product /x/ by product /y/ (assuming that /x/ is divisible by /y/).
class                                             Divide x y z | x y -> z
instance (C x y c, D c x y z', Normalise z' z) => Divide x y z

-- | Finds the least common multiple of product /x/ and product /y/.
class                                             LCM x y z | x y -> z
instance (C x y c, L c x y z', Normalise z' z) => LCM x y z

-- | Finds the greatest common divisor of product /x/ and product /y/.
class                                             GCD x y z | x y -> z
instance (C x y c, G c x y z', Normalise z' z) => GCD x y z

-- | Multiplies product /x/ with product /y/, where the heads
--   /a/ and /b/ of products /x/ and /y/ have relative ordering /c/.
class                                                             M  c          x           y           z | c x y -> z
instance                                                          M EQ          E           E           E
instance                                                          M LT          E  (b:^:q:::y) (b:^:q:::y)
instance                                                          M GT (a:^:p:::x)          E  (a:^:p:::x)
instance (C  x          y  c, M c  x          y  z, Add p q r) => M EQ (a:^:p:::x) (a:^:q:::y) (a:^:r:::z)
instance (C  x (b:^:q:::y) c, M c  x (b:^:q:::y) z           ) => M LT (a:^:p:::x) (b:^:q:::y) (a:^:p:::z)
instance (C (a:^:p:::x) y  c, M c (a:^:p:::x) y  z           ) => M GT (a:^:p:::x) (b:^:q:::y) (b:^:q:::z)

-- | Divides product /x/ by product /y/ (assuming that /x/ is divisible by /y/),
--   where the heads /a/ and /b/ of products /x/ and /y/ have relative ordering /c/.
class                                                             D  c          x           y           z | c x y -> z
instance                                                          D EQ          E           E           E
instance                                                          D LT          E  (b:^:q:::y) (b:^:q:::y)
instance                                                          D GT (a:^:p:::x)          E  (a:^:p:::x)
instance (C  x          y  c, D c  x          y  z, Sub p q r) => D EQ (a:^:p:::x) (a:^:q:::y) (a:^:r:::z)
instance (C  x (b:^:q:::y) c, D c  x (b:^:q:::y) z           ) => D LT (a:^:p:::x) (b:^:q:::y) (a:^:p:::z)
instance (C (a:^:p:::x) y  c, D c (a:^:p:::x) y  z           ) => D GT (a:^:p:::x) (b:^:q:::y) (b:^:q:::z)

-- | Finds the least common multiple of product /x/ and product /y/, where
--   the heads /a/ and /b/ of products /x/ and /y/ have relative ordering /c/.
class                                                             L  c          x           y           z | c x y -> z
instance                                                          L EQ          E           E           E
instance                                                          L LT          E  (b:^:q:::y) (b:^:q:::y)
instance                                                          L GT (a:^:p:::x)          E  (a:^:p:::x)
instance (C  x          y  c, L c  x          y  z, Max p q r) => L EQ (a:^:p:::x) (a:^:q:::y) (a:^:r:::z)
instance (C  x (b:^:q:::y) c, L c  x (b:^:q:::y) z           ) => L LT (a:^:p:::x) (b:^:q:::y) (a:^:p:::z)
instance (C (a:^:p:::x) y  c, L c (a:^:p:::x) y  z           ) => L GT (a:^:p:::x) (b:^:q:::y) (b:^:q:::z)

-- | Finds the greatest common divisor of product /x/ and product /y/, where
--   the heads /a/ and /b/ of products /x/ and /y/ have relative ordering /c/.
class                                                             G  c          x           y           z | c x y -> z
instance                                                          G EQ          E           E           E
instance                                                          G LT          E  (b:^:q:::y)          E
instance                                                          G GT (a:^:p:::x)          E           E
instance (C  x          y  c, G c  x          y  z, Min p q r) => G EQ (a:^:p:::x) (a:^:q:::y) (a:^:r:::z)
instance (C  x (b:^:q:::y) c, G c  x (b:^:q:::y) z           ) => G LT (a:^:p:::x) (b:^:q:::y)          z
instance (C (a:^:p:::x) y  c, G c (a:^:p:::x) y  z           ) => G GT (a:^:p:::x) (b:^:q:::y)          z

-- | Compares the head (/a/ ^ /p/) of product /x/
--   with the head (/b/ ^ /q/) of product /y/.
--
--   Returns:
--
--   * 'LT' if (/a/ \< /b/)
--
--   * 'GT' if (/a/  > /b/)
--
--   * 'EQ' if (/a/  = /b/).
--
class                                   C              x               y   c | x y -> c
instance                                C              E               E  EQ
instance                                C              E  (   b :^:q:::y) LT
instance                                C (   a :^:p:::x)              E  GT
instance                                C (   Z :^:p:::x) (   Z :^:q:::y) EQ
instance                                C (   Z :^:p:::x) ((P b):^:q:::y) LT
instance                                C ((P a):^:p:::x) (   Z :^:q:::y) GT
instance C (a:^:p:::x) (b:^:q:::y) z => C ((P a):^:p:::x) ((P b):^:q:::y) z

