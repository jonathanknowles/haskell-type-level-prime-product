{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Type-level encodings and operations for products of prime numbers.
module Data.TypeLevel.PrimeProduct.Dense
	( (:::)
	, E
	, Multiply
	, Divide
	, LCM
	, GCD
	, Reciprocal
	, Extend
	)
	where

import Data.TypeLevel.Comparison
import Data.TypeLevel.Integer

infixr 0 :::

-- | A type-level representation of a /prime product/
--   encoded as a list of prime exponents [  /p/,   /q/,   /r/ ... ]
--   that represents the prime product    [2^/p/, 3^/q/, 5^/r/ ... ].
data primeExponent ::: tail

-- | A type-level representation of the /empty/ prime product.
data E

-- | Multiplies product /x/ with product /y/.
class                                 Multiply x y z | x y -> z
instance (M x y z', TrimTail z' z) => Multiply x y z

-- | Divides product /x/ by product /y/.
class                                 Divide x y z | x y -> z
instance (D x y z', TrimTail z' z) => Divide x y z

-- | Finds the least common multiple of product /x/ and product /y/.
class                                 LCM x y z | x y -> z
instance (L x y z', TrimTail z' z) => LCM x y z

-- | Finds the greatest common divisor of product /x/ and product /y/.
class                                 GCD x y z | x y -> z
instance (G x y z', TrimTail z' z) => GCD x y z

-- | Find the reciprocal of product /x/.
class                                    Reciprocal      x       y | x -> y
instance                                 Reciprocal      E       E
instance (Negate p q, Reciprocal x y) => Reciprocal (p:::x) (q:::y)

-- | Reverses the list that encodes product /x/.
class               Reverse x y | x -> y
instance R x E y => Reverse x y

-- | Removes the longest-possible consecutive sequence of zero exponents from the tail of the list that encodes product /x/.
class                                                TrimTail x y | x -> y
instance (Reverse x a, TrimHead a b, Reverse b y) => TrimTail x y

-- | Removes the longest-possible consecutive sequence of zero exponents from the head of the list that encodes product /x/.
class                    TrimHead          x           y | x -> y
instance                 TrimHead          E           E
instance TrimHead x y => TrimHead (   Z :::x)          y
instance                 TrimHead ((N a):::x) ((N a):::x)
instance                 TrimHead ((P a):::x) ((P a):::x)

-- | Multiplies product /x/ with product /y/, producing a result
--   that may contain trailing zero exponents.
class                            M      x       y       z | x y -> z
instance                         M      E       E       E
instance (Add Z q r, M E y z) => M      E  (q:::y) (r:::z)
instance (Add p Z r, M x E z) => M (p:::x)      E  (r:::z)
instance (Add p q r, M x y z) => M (p:::x) (q:::y) (r:::z)

-- | Multiplies product /x/ with product /y/, producing a result
--   that may contain trailing zero exponents.
class                            D      x       y       z | x y -> z
instance                         D      E       E       E
instance (Sub Z q r, D E y z) => D      E  (q:::y) (r:::z)
instance (Sub p Z r, D x E z) => D (p:::x)      E  (r:::z)
instance (Sub p q r, D x y z) => D (p:::x) (q:::y) (r:::z)

-- | Finds the least common multiple of product /x/ and product /y/,
--   producing a result that may contain trailing zero exponents.
class                            L      x       y       z | x y -> z
instance                         L      E       E       E
instance (Max Z q r, L E y z) => L      E  (q:::y) (r:::z)
instance (Max p Z r, L x E z) => L (p:::x)      E  (r:::z)
instance (Max p q r, L x y z) => L (p:::x) (q:::y) (r:::z)

-- | Finds the greatest common divisor of product /x/ and product /y/,
--   producing a result that may contain trailing zero exponents.
class                            G      x       y       z | x y -> z
instance                         G      E       E       E
instance (Min Z q r, G E y z) => G      E  (q:::y) (r:::z)
instance (Min p Z r, G x E z) => G (p:::x)      E  (r:::z)
instance (Min p q r, G x y z) => G (p:::x) (q:::y) (r:::z)

-- | Reverses the list that encodes product /x/, using the accumulator /a/.
class                     R      x  a y | x a -> y
instance                  R      E  a a
instance R x (p:::a) z => R (p:::x) a z

-- | Extends at most one of the pair of products /x/ and /y/, so that
--   their encodings are the same length as one another. The extended
--   product is padded with zero exponentials.
class Extend x x' y y' | x y -> x' y'

instance Extend E E E E

instance Extend a a  E b  => Extend (p:::a) (p:::a )      E  (Z:::b )
instance Extend E a  b b  => Extend      E  (Z:::a ) (q:::b) (q:::b )
instance Extend a a' b b' => Extend (p:::a) (p:::a') (q:::b) (q:::b')
