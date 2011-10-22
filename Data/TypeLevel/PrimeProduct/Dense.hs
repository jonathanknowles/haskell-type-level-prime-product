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
class Multiply x y z | x y -> z

-- | Divides product /x/ by product /y/.
class Divide x y z | x y -> z

-- | Finds the least common multiple of product /x/ and product /y/.
class LCM x y z | x y -> z

-- | Finds the greatest common divisor of product /x/ and product /y/.
class GCD x y z | x y -> z

-- Uses binary operator /f/ to zip together products /x/ and /y/.
class Zip f x y z | f x y -> z

-- Uses binary operator /f/ to zip together extended products /x/ and /y/.
class Zip' f x y z | f x y -> z

instance (Zip AddOperator x y z) => Multiply x y z
instance (Zip SubOperator x y z) => Divide   x y z
instance (Zip MaxOperator x y z) => LCM      x y z
instance (Zip MinOperator x y z) => GCD      x y z

instance (Extend x x' y y', Zip' f x' y' z', TrimTail z' z) => Zip f x y z

instance                                        Zip' f      E       E       E
instance (ApplyBinary f p q r, Zip' f x y z) => Zip' f (p:::x) (q:::y) (r:::z)

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
