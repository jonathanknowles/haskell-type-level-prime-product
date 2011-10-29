{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.TypeLevel.PrimeProduct
	( module Data.TypeLevel.Integer
	, module Data.TypeLevel.List
	, module Data.TypeLevel.Number
	, module Data.TypeLevel.Operator
	, module Data.TypeLevel.Ordering
	, Contract
	, Expand
	, ZipContracted
	, ZipExpanded
	) where

import Data.TypeLevel.Integer
import Data.TypeLevel.List
import Data.TypeLevel.Number
import Data.TypeLevel.Operator
import Data.TypeLevel.Ordering

-- | Contracts product /x/ to its shortest possible representation.
class Contract x x' | x -> x'

-- | Expands either or both of products /x/ and /y/ so that their
--   representations are the same length.
class Expand x x' y y' | x y -> x' y'

-- | Uses binary operator /f/ to zip together products /x/ and /y/
--   where /x/ and /y/ are contracted representations.
class ZipContracted f x y z | f x y -> z

-- | Uses binary operator /f/ to zip together products /x/ and /y/
--   where /x/ and /y/ are expanded representations.
class ZipExpanded f x y z | f x y -> z

instance (Expand x x' y y', ZipExpanded f x' y' z', Contract z' z) => ZipContracted f x y z

instance (ZipContracted OperatorAdd      x y z) => Multiply x y z
instance (ZipContracted OperatorSubtract x y z) => Divide   x y z
instance (ZipContracted OperatorMaximum  x y z) => LCM      x y z
instance (ZipContracted OperatorMinimum  x y z) => GCD      x y z
