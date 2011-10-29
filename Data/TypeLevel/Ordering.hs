{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.TypeLevel.Ordering
	( EQ
	, LT
	, GT
	, Compare , OperatorCompare
	, Maximum , OperatorMaximum
	, Minimum , OperatorMinimum
	) where

import Data.TypeLevel.Operator

-- | The result of a comparison between /a/ and /b/ such that (/a/ = /b/).
data EQ

-- | The result of a comparison between /a/ and /b/ such that (/a/ \< /b/).
data LT

-- | The result of a comparison between /a/ and /b/ such that (/a/ > /b/).
data GT

-- | Compares /a/ with /b/ to find their relative ordering /c/ such that /a/ /b/ /c/.
class ComparisonResult c => Compare a b c | a b -> c

-- | The result of a comparison between two types.
class ComparisonResult c
instance ComparisonResult EQ
instance ComparisonResult LT
instance ComparisonResult GT

-- | Finds the greater of /a/ and /b/.
class Maximum a b c | a b -> c
instance (Compare a b z, Maximum' z a b c) => Maximum a b c

-- | Finds the greater of /a/ and /b/ when the relative ordering /z/ between /a/ and /b/ is known.
class    Maximum' z  a b c | z a b -> c
instance Maximum' EQ a a a
instance Maximum' LT a b b
instance Maximum' GT a b a

-- | Finds the smaller of /a/ and /b/.
class Minimum a b c | a b -> c
instance (Compare a b z, Minimum' z a b c) => Minimum a b c

-- | Finds the smaller of /a/ and /b/ when the relative ordering /z/ between /a/ and /b/ is known.
class    Minimum' z  a b c | z a b -> c
instance Minimum' EQ a a a
instance Minimum' LT a b a
instance Minimum' GT a b b

data OperatorCompare
data OperatorMaximum
data OperatorMinimum

instance Compare a b c => ApplyBinary OperatorCompare a b c
instance Minimum a b c => ApplyBinary OperatorMinimum a b c
instance Maximum a b c => ApplyBinary OperatorMaximum a b c
