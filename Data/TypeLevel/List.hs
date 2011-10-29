{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.TypeLevel.List
	( (:::)
	, E
	, Reverse
	) where

infixr 0 :::

-- | A type-level encoding of a list node.
data head ::: tail

-- | A type-level encoding of the empty list.
data E

-- | Reverses list /x/ to produce list /y/.
class               Reverse x y | x -> y
instance R x E y => Reverse x y

-- | Reverses list /x/ to produce list /y/ using accumulator /a/.
class                     R      x  a y | x a -> y
instance                  R      E  a a
instance R x (p:::a) z => R (p:::x) a z
