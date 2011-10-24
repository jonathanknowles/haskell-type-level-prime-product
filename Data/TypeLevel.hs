{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.TypeLevel
	( Compare
	, EQ
	, LT
	, GT
	, Maximum
	, Minimum
	, Negate
	, Add
	, Subtract
	, Multiply
	, Divide
	, LCM
	, GCD
	, Reciprocal
	, ApplyBinary
	, OperatorAdd
	, OperatorSubtract
	, OperatorMaximum
	, OperatorMinimum
	) where

-- | Compares /a/ with /b/ to find their relative order /c/.
--
--   Returns:
--
--   * 'EQ' if (/a/  = /b/).
--
--   * 'LT' if (/a/ \< /b/).
--
--   * 'GT' if (/a/  > /b/).
--
class Compare a b c | a b -> c

-- | The result of a comparison between /a/ and /b/ such that (/a/ = /b/).
data EQ

-- | The result of a comparison between /a/ and /b/ such that (/a/ \< /b/).
data LT

-- | The result of a comparison between /a/ and /b/ such that (/a/ > /b/).
data GT

-- | Finds the greater of /a/ and /b/.
class Maximum a b c | a b -> c
instance (Compare a b z, Maximum' z a b c) => Maximum a b c

-- | Finds the smaller of /a/ and /b/.
class Minimum a b c | a b -> c
instance (Compare a b z, Minimum' z a b c) => Minimum a b c

-- | Finds the negation /b/ of /a/.
class Negate a b | a -> b

-- | Adds /a/ to /b/ to produce /c/.
class Add a b c | a b -> c

-- | Subtracts /b/ from /a/ to produce /c/.
class Subtract a b c | a b -> c
instance (Negate b b', Add a b' c) => Subtract a b c

-- | Multiplies /a/ with /b/ to produce /c/.
class Multiply a b c | a b -> c

-- | Divides /a/ by /b/ to produce /c/.
class Divide a b c | a b -> c

-- | Finds the least common multiple /c/ of /a/ and /b/.
class LCM a b c | a b -> c

-- | Finds the greatest common divisor /c/ of /a/ and /b/.
class GCD a b c | a b -> c

-- | Finds the reciprocal /b/ of /a/.
class Reciprocal a b | a -> b

-- Apply binary operator /f/ to /a/ and /b/ to produce /c/.
class ApplyBinary f a b c | f a b -> c

data OperatorAdd
data OperatorSubtract
data OperatorMaximum
data OperatorMinimum

instance Add      a b c => ApplyBinary OperatorAdd      a b c
instance Subtract a b c => ApplyBinary OperatorSubtract a b c
instance Minimum  a b c => ApplyBinary OperatorMinimum  a b c
instance Maximum  a b c => ApplyBinary OperatorMaximum  a b c

-- | Finds the greater of /a/ and /b/ when the relative ordering /z/ between /a/ and /b/ is known.
class    Maximum' z  a b c | z a b -> c
instance Maximum' EQ a a a
instance Maximum' LT a b b
instance Maximum' GT a b a

-- | Finds the smaller of /a/ and /b/ when the relative ordering /z/ between /a/ and /b/ is known.
class    Minimum' z  a b c | z a b -> c
instance Minimum' EQ a a a
instance Minimum' LT a b a
instance Minimum' GT a b b

