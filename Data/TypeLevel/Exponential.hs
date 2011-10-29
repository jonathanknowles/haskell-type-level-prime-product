{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators  #-}

module Data.TypeLevel.Exponential
	( (:^:) )
	where

infixl 1 :^:

-- | A type-level representation of an /exponential/ : a number raised to a power.
data base :^: exponent
