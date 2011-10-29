{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.TypeLevel.Number
	( Negate     , OperatorNegate
	, Reciprocal , OperatorReciprocal
	, Add        , OperatorAdd
	, Subtract   , OperatorSubtract
	, Multiply   , OperatorMultiply
	, Divide     , OperatorDivide
	, LCM        , OperatorLCM
	, GCD        , OperatorGCD
	) where

import Data.TypeLevel.Operator

-- | Finds the negation /b/ of /a/.
class Negate a b | a -> b

-- | Finds the reciprocal /b/ of /a/.
class Reciprocal a b | a -> b

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

data OperatorAdd
data OperatorSubtract
data OperatorMultiply
data OperatorDivide
data OperatorLCM
data OperatorGCD

instance Add      a b c => ApplyBinary OperatorAdd      a b c
instance Subtract a b c => ApplyBinary OperatorSubtract a b c
instance Multiply a b c => ApplyBinary OperatorMultiply a b c
instance Divide   a b c => ApplyBinary OperatorDivide   a b c
instance LCM      a b c => ApplyBinary OperatorLCM      a b c
instance GCD      a b c => ApplyBinary OperatorGCD      a b c
