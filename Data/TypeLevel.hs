{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.TypeLevel
	( EQ
	, LT
	, GT
	, ApplyUnary
	, ApplyBinary
	, Negate     , OperatorNegate
	, Reciprocal , OperatorReciprocal
	, Compare    , OperatorCompare
	, Maximum    , OperatorMaximum
	, Minimum    , OperatorMinimum
	, Add        , OperatorAdd
	, Subtract   , OperatorSubtract
	, Multiply   , OperatorMultiply
	, Divide     , OperatorDivide
	, LCM        , OperatorLCM
	, GCD        , OperatorGCD
	) where

-- | The result of a comparison between /a/ and /b/ such that (/a/ = /b/).
data EQ

-- | The result of a comparison between /a/ and /b/ such that (/a/ \< /b/).
data LT

-- | The result of a comparison between /a/ and /b/ such that (/a/ > /b/).
data GT

-- Apply unary operator /f/ to /a/ to produce /b/.
class ApplyUnary f a b | f a -> b

-- Apply binary operator /f/ to /a/ and /b/ to produce /c/.
class ApplyBinary f a b c | f a b -> c

-- | Finds the negation /b/ of /a/.
class Negate a b | a -> b

-- | Finds the reciprocal /b/ of /a/.
class Reciprocal a b | a -> b

-- | Compares /a/ with /b/ to find their relative order /c/.
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

-- | Adds /a/ to /b/ to produce /c/.
class Add a b c | a b -> c

-- | Subtracts /b/ from /a/ to produce /c/.
class Subtract a b c | a b -> c

-- | Multiplies /a/ with /b/ to produce /c/.
class Multiply a b c | a b -> c

-- | Divides /a/ by /b/ to produce /c/.
class Divide a b c | a b -> c

-- | Finds the least common multiple /c/ of /a/ and /b/.
class LCM a b c | a b -> c

-- | Finds the greatest common divisor /c/ of /a/ and /b/.
class GCD a b c | a b -> c

data OperatorNegate
data OperatorReciprocal

instance Negate     a b => ApplyUnary OperatorNegate     a b
instance Reciprocal a b => ApplyUnary OperatorReciprocal a b

data OperatorCompare
data OperatorAdd
data OperatorSubtract
data OperatorMaximum
data OperatorMinimum
data OperatorMultiply
data OperatorDivide
data OperatorLCM
data OperatorGCD

instance Compare  a b c => ApplyBinary OperatorCompare  a b c
instance Add      a b c => ApplyBinary OperatorAdd      a b c
instance Subtract a b c => ApplyBinary OperatorSubtract a b c
instance Minimum  a b c => ApplyBinary OperatorMinimum  a b c
instance Maximum  a b c => ApplyBinary OperatorMaximum  a b c
instance Multiply a b c => ApplyBinary OperatorMultiply a b c
instance Divide   a b c => ApplyBinary OperatorDivide   a b c
instance LCM      a b c => ApplyBinary OperatorLCM      a b c
instance GCD      a b c => ApplyBinary OperatorGCD      a b c

