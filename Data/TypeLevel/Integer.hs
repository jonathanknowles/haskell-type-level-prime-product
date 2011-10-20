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
data S x

-- | Adds integer /a/ to integer /b/.
class                   Add    a  b    c | a b -> c
instance                Add    Z  b    b
instance (Add a b c) => Add (S a) b (S c)

-- | Subtracts integer /b/ from integer /a/ (assuming that /a/ >= /b/).
class                   Sub    a     b  c | a b -> c
instance                Sub    a     Z  a
instance (Sub a b c) => Sub (S a) (S b) c

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
instance                  Compare    Z  (S b) LT
instance                  Compare (S a)    Z  GT
instance Compare a b c => Compare (S a) (S b)  z

-- | Finds the smaller of two integers /a/ and /b/.
class                 Min    a     b     c | a b -> c
instance              Min    Z     Z     Z
instance              Min    Z  (S b)    Z
instance              Min (S a)    Z     Z
instance Min a b c => Min (S a) (S b) (S c)

-- | Finds the greater of two integers /a/ and /b/.
class                 Max    a     b     c | a b -> c
instance              Max    Z     Z     Z
instance              Max    Z  (S b) (S b)
instance              Max (S a)    Z  (S a)
instance Max a b c => Max (S a) (S b) (S c)

