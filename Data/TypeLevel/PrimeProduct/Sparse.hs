{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Type-level encodings and operations for products of prime numbers.
module Data.TypeLevel.PrimeProduct.Sparse
	( module Data.TypeLevel.Exponential
	, module Data.TypeLevel.PrimeProduct
	) where

import Data.TypeLevel.Exponential
import Data.TypeLevel.PrimeProduct

instance ZipExpanded f E E E
instance (ApplyBinary f p q r, ZipExpanded f x y z) => ZipExpanded f (a:^:p:::x) (a:^:q:::y) (a:^:r:::z)

instance Reciprocal E E
instance (Negate p q, Reciprocal x y) => Reciprocal (a:^:p:::x) (a:^:q:::y)

instance Contract E E
instance Contract x y => Contract (a:^:(N n) ::: x) (a:^:(N n) ::: y)
instance Contract x y => Contract (a:^:(P n) ::: x) (a:^:(P n) ::: y)
instance Contract x y => Contract (a:^:   Z  ::: x)                y

instance Expand E E E E
instance Expand x x E y => Expand (a:^:p:::x) (a:^:p:::x)          E  (a:^:Z:::y)
instance Expand E x y y => Expand          E  (b:^:Z:::x) (b:^:q:::y) (b:^:q:::y)

instance (Compare a b c, Expand' c (a:^:p:::x) x' (b:^:q:::y) y') => Expand (a:^:p:::x) x' (b:^:q:::y) y'

-- | Expands non-empty products /x/ and /y/ with leading exponentials
--   /a/^/p/ and /b/^/q/ where /a/ and /b/ have relative ordering /c/
--   into products /x'/ and /y'/, whose encodings are the same length
--   and share the same set of prime bases as one another.
class Expand' c x x' y y' | c x y -> x' y'

instance Expand          x  x'          y  y' => Expand' EQ (a:^:p:::x) (a:^:p:::x') (b:^:q:::y) (b:^:q:::y')
instance Expand          x  x' (b:^:q:::y) y' => Expand' LT (a:^:p:::x) (a:^:p:::x') (b:^:q:::y) (a:^:Z:::y')
instance Expand (a:^:p:::x) x'          y  y' => Expand' GT (a:^:p:::x) (b:^:Z:::x') (b:^:q:::y) (b:^:q:::y')
