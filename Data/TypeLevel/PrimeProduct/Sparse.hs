{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Type-level encodings and operations for products of prime numbers.
module Data.TypeLevel.PrimeProduct.Sparse
	( (:::), (:^:)
	, E
	, Value
	, V
	)
	where

import Data.TypeLevel
import Data.TypeLevel.Integer

infixr 0 :::
infixl 1 :^:

-- | A type-level representation of an /exponentiated prime number/ : a prime number raised to a positive integer power.
data prime :^: exponent

-- | A type-level representation of a /prime product/ : a list of exponentiated prime numbers in ascending order [2^/p/, 3^/q/, 5^/r/ ... ].
data exponentiatedPrime ::: tail

-- | A type-level representation of the /empty/ prime product.
data E

-- Uses binary operator /f/ to zip together products /x/ and /y/.
class Zip f x y z | f x y -> z

-- Uses binary operator /f/ to zip together expanded products /x/ and /y/.
class Zip' f x y z | f x y -> z

instance (Zip OperatorAdd x y z) => Multiply x y z
instance (Zip OperatorSub x y z) => Divide   x y z
instance (Zip OperatorMax x y z) => LCM      x y z
instance (Zip OperatorMin x y z) => GCD      x y z

instance (Expand x x' y y', Zip' f x' y' z', Contract z' z) => Zip f x y z

instance                                        Zip' f          E           E           E
instance (ApplyBinary f p q r, Zip' f x y z) => Zip' f (a:^:p:::x) (a:^:q:::y) (a:^:r:::z)

instance                                 Reciprocal          E           E
instance (Negate p q, Reciprocal x y) => Reciprocal (a:^:p:::x) (a:^:q:::y)

-- | Contracts product /x/ by removing all factors with a zero exponent.
class                    Contract                x                 y | x -> y
instance                 Contract                E                 E
instance Contract x y => Contract (a:^:(N n) ::: x) (a:^:(N n) ::: y)
instance Contract x y => Contract (a:^:(P n) ::: x) (a:^:(P n) ::: y)
instance Contract x y => Contract (a:^:   Z  ::: x)                y

-- | Expands products /x/ and /y/ into products /x'/ and /y'/ whose
--   encodings are the same length and share the same set of prime
--   bases as one another.
class Expand x x' y y' | x y -> x' y'

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

-- | The result of evaluating type-level value /x/ at run-time.
data V x = V Integer

-- | Converts product /x/ into a run-time value.
class                          Value      x  where value :: V x
instance                       Value (    Z) where value  = V (    0)
instance (Value t         ) => Value (N   t) where value  = V (x - 1) where V x = value :: V t
instance (Value t         ) => Value (P   t) where value  = V (x + 1) where V x = value :: V t
instance                       Value (    E) where value  = V (    1)
instance (Value a, Value p) => Value (a:^:p) where value  = V (a ^ p) where (V a, V p) = (value, value) :: (V a, V p)
instance (Value x, Value y) => Value (x:::y) where value  = V (x * y) where (V x, V y) = (value, value) :: (V x, V y)

