{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Type-level encodings and operations for integers.
module Data.TypeLevel.Integer where

import Data.TypeLevel.Comparison

-- | A type-level representation of the integer /zero/.
data Z

-- | A type-level representation of a /positive integer/.
data P x

-- | Adds integer /a/ to integer /b/.
class                   Add    a  b    c | a b -> c
instance                Add    Z  b    b
instance (Add a b c) => Add (P a) b (P c)

-- | Subtracts integer /b/ from integer /a/ (assuming that /a/ >= /b/).
class                   Sub    a     b  c | a b -> c
instance                Sub    a     Z  a
instance (Sub a b c) => Sub (P a) (P b) c

-- | Compares integer /a/ to integer /b/.
--
--   Returns:
--
--   * 'LT' if (/a/ \< /b/)
--
--   * 'GT' if (/a/  > /b/)
--
--   * 'EQ' if (/a/  = /b/).
--
class                     Compare    a     b   c | a b -> c
instance                  Compare    Z     Z  EQ
instance                  Compare    Z  (P b) LT
instance                  Compare (P a)    Z  GT
instance Compare a b c => Compare (P a) (P b)  z

-- | Finds the smaller of two integers /a/ and /b/.
class                 Min    a     b     c | a b -> c
instance              Min    Z     Z     Z
instance              Min    Z  (P b)    Z
instance              Min (P a)    Z     Z
instance Min a b c => Min (P a) (P b) (P c)

-- | Finds the greater of two integers /a/ and /b/.
class                 Max    a     b     c | a b -> c
instance              Max    Z     Z     Z
instance              Max    Z  (P b) (P b)
instance              Max (P a)    Z  (P a)
instance Max a b c => Max (P a) (P b) (P c)

