{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Type-level encodings and operations for integers.
module Data.TypeLevel.Integer
	( Z
	, N
	, P
	, Compare
	, Negate
	, Add
	, Sub
	, Max
	, Min
	, AddOperator
	, SubOperator
	, MaxOperator
	, MinOperator
	, ApplyBinary
	)
	where

import Data.TypeLevel.Comparison

-- | A type-level representation of the integer /zero/.
data Z

-- | A type-level representation of a /negative integer/.
data N x

-- | A type-level representation of a /positive integer/.
data P x

-- | Adds integer /a/ to integer /b/.
class                   Add    a     b        c | a b -> c
instance                Add    Z     Z        Z
instance                Add    a     Z        a
instance                Add    Z     b        b
instance (Add a b c) => Add (N a) (N b) (N (N c))
instance (Add a b c) => Add (N a) (P b)       c
instance (Add a b c) => Add (P a) (N b)       c
instance (Add a b c) => Add (P a) (P b) (P (P c))

-- | Subtracts integer /b/ from integer /a/.
class                                 Sub a b c | a b -> c
instance (Negate b b', Add a b' c) => Sub a b c

-- | Finds the greater of two integers /a/ and /b/.
class                                     Max a b c | a b -> c
instance (Compare a b z, Max' z a b c) => Max a b c

-- | Finds the smaller of two integers /a/ and /b/.
class                                     Min a b c | a b -> c
instance (Compare a b z, Min' z a b c) => Min a b c

-- | Compares integer /a/ with integer /b/.
--
--   Returns:
--
--   * 'EQ' if (/a/  = /b/).
--
--   * 'LT' if (/a/ \< /b/).
--
--   * 'GT' if (/a/  > /b/).
--
class                     Compare    a     b   c | a b -> c
instance                  Compare    Z     Z  EQ
instance                  Compare    Z  (N b) GT
instance                  Compare    Z  (P b) LT
instance                  Compare (N a)    Z  LT
instance                  Compare (P a)    Z  GT
instance Compare a b c => Compare (N a) (N b)  c
instance                  Compare (N a) (P b) LT
instance                  Compare (P a) (N b) GT
instance Compare a b c => Compare (P a) (P b)  c

-- | Negates integer /a/.
class                  Negate    a     b | a -> b
instance               Negate    Z     Z
instance Negate a b => Negate (N a) (P b)
instance Negate a b => Negate (P a) (N b)

-- | Finds the greater of two integers /a/ and /b/ when the relative ordering /z/ between /a/ and /b/ is known.
class    Max' z  a b c | z a b -> c
instance Max' EQ a a a
instance Max' LT a b b
instance Max' GT a b a

-- | Finds the smaller of two integers /a/ and /b/ when the relative ordering /z/ between /a/ and /b/ is known.
class    Min' z  a b c | z a b -> c
instance Min' EQ a a a
instance Min' LT a b a
instance Min' GT a b b

-- Apply binary operator /f/ to integers /a/ and /b/.
class ApplyBinary f a b c | f a b -> c

data AddOperator
data SubOperator
data MaxOperator
data MinOperator

instance Add a b c => ApplyBinary AddOperator a b c
instance Sub a b c => ApplyBinary SubOperator a b c
instance Min a b c => ApplyBinary MinOperator a b c
instance Max a b c => ApplyBinary MaxOperator a b c

