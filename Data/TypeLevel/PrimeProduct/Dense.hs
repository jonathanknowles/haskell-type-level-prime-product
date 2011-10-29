{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Type-level encodings and operations for products of prime numbers.
module Data.TypeLevel.PrimeProduct.Dense
	( module Data.TypeLevel.PrimeProduct
	) where

import Data.TypeLevel.PrimeProduct

instance                                               ZipExpanded f      E       E       E
instance (ApplyBinary f p q r, ZipExpanded f x y z) => ZipExpanded f (p:::x) (q:::y) (r:::z)

instance                                 Reciprocal      E       E
instance (Negate p q, Reciprocal x y) => Reciprocal (p:::x) (q:::y)

instance TrimTail x y => Contract x y

-- | Removes the longest-possible consecutive sequence of zero exponents
--   from the head of the list that encodes product /x/.
class                    TrimHead          x           y | x -> y
instance                 TrimHead          E           E
instance TrimHead x y => TrimHead (   Z :::x)          y
instance                 TrimHead ((N a):::x) ((N a):::x)
instance                 TrimHead ((P a):::x) ((P a):::x)

-- | Removes the longest-possible consecutive sequence of zero exponents
--   from the tail of the list that encodes product /x/.
class                                                TrimTail x y | x -> y
instance (Reverse x a, TrimHead a b, Reverse b y) => TrimTail x y

instance Expand E E E E
instance Expand a a  E b  => Expand (p:::a) (p:::a )      E  (Z:::b )
instance Expand E a  b b  => Expand      E  (Z:::a ) (q:::b) (q:::b )
instance Expand a a' b b' => Expand (p:::a) (p:::a') (q:::b) (q:::b')
