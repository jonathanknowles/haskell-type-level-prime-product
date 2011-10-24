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
	, Max
	, Min
	, Negate
	, Add
	, Sub
	, Multiply
	, Divide
	, LCM
	, GCD
	, Reciprocal
	, ApplyBinary
	, AddOperator
	, SubOperator
	, MaxOperator
	, MinOperator
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
class Max a b c | a b -> c
instance (Compare a b z, Max' z a b c) => Max a b c

-- | Finds the smaller of /a/ and /b/.
class Min a b c | a b -> c
instance (Compare a b z, Min' z a b c) => Min a b c

-- | Finds the negation /b/ of /a/.
class Negate a b | a -> b

-- | Adds /a/ to /b/ to produce /c/.
class Add a b c | a b -> c

-- | Subtracts /b/ from /a/ to produce /c/.
class Sub a b c | a b -> c
instance (Negate b b', Add a b' c) => Sub a b c

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

data AddOperator
data SubOperator
data MaxOperator
data MinOperator

instance Add a b c => ApplyBinary AddOperator a b c
instance Sub a b c => ApplyBinary SubOperator a b c
instance Min a b c => ApplyBinary MinOperator a b c
instance Max a b c => ApplyBinary MaxOperator a b c

-- | Finds the greater of /a/ and /b/ when the relative ordering /z/ between /a/ and /b/ is known.
class    Max' z  a b c | z a b -> c
instance Max' EQ a a a
instance Max' LT a b b
instance Max' GT a b a

-- | Finds the smaller of /a/ and /b/ when the relative ordering /z/ between /a/ and /b/ is known.
class    Min' z  a b c | z a b -> c
instance Min' EQ a a a
instance Min' LT a b a
instance Min' GT a b b

