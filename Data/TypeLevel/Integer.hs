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
	)
	where

import Data.TypeLevel

-- | A type-level representation of the integer /zero/.
data Z

-- | A type-level representation of a /negative integer/.
data N x

-- | A type-level representation of a /positive integer/.
data P x

instance                Add    Z     Z        Z
instance                Add    a     Z        a
instance                Add    Z     b        b
instance (Add a b c) => Add (N a) (N b) (N (N c))
instance (Add a b c) => Add (N a) (P b)       c
instance (Add a b c) => Add (P a) (N b)       c
instance (Add a b c) => Add (P a) (P b) (P (P c))

instance                  Compare    Z     Z  EQ
instance                  Compare    Z  (N b) GT
instance                  Compare    Z  (P b) LT
instance                  Compare (N a)    Z  LT
instance                  Compare (P a)    Z  GT
instance                  Compare (N a) (P b) LT
instance                  Compare (P a) (N b) GT
instance Compare a b c => Compare (P a) (P b)  c
instance Compare a b c => Compare (N a) (N b)  c

instance               Negate    Z     Z
instance Negate a b => Negate (N a) (P b)
instance Negate a b => Negate (P a) (N b)

