{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Data.TypeLevel.Operator
	( ApplyUnary
	, ApplyBinary
	) where

-- Applies unary operator /f/ to /a/ to produce /b/.
class ApplyUnary f a b | f a -> b

-- Applies binary operator /f/ to /a/ and /b/ to produce /c/.
class ApplyBinary f a b c | f a b -> c
