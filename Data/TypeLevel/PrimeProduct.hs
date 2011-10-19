{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Type-level encodings and operations for products of prime numbers.
module Data.TypeLevel.PrimeProduct where

infixr 0 :::
infixl 1 :^:

-- | A type-level representation of the integer /zero/.
data Z

-- | A type-level representation of a /positive integer/.
data S x

-- | A type-level representation of an /exponentiated prime number/ : a prime number raised to a positive integer power.
data prime :^: exponent

-- | A type-level representation of a /prime product/ : a list of exponentiated prime numbers in ascending order [2^/p/, 3^/q/, 5^/r/ ... ].
data exponentiatedPrime ::: tail

-- | A type-level representation of the /empty/ prime product.
data N

-- | The result of a type-level comparison between /a/ and /b/ such that (/a/ = /b/).
data EQ

-- | The result of a type-level comparison between /a/ and /b/ such that (/a/ \< /b/).
data LT

-- | The result of a type-level comparison between /a/ and /b/ such that (/a/ > /b/).
data GT

-- | The result of evaluating type-level value /x/ at run-time.
data V x = V Integer

-- | Adds integer /a/ to integer /b/.
class                   Add    a  b    c | a b -> c
instance                Add    Z  b    b
instance (Add a b c) => Add (S a) b (S c)

-- | Subtracts integer /b/ from integer /a/ (assuming that /a/ >= /b/).
class                   Sub    a     b  c | a b -> c
instance                Sub    a     Z  a
instance (Sub a b c) => Sub (S a) (S b) c

-- | Converts product /x/ into a run-time value.
class                          Value      x  where value :: V x
instance                       Value (    Z) where value  = V (    0)
instance (Value t         ) => Value (S   t) where value  = V (x + 1) where V x = value :: V t
instance                       Value (    N) where value  = V (    1)
instance (Value a, Value p) => Value (a:^:p) where value  = V (a ^ p) where (V a, V p) = (value, value) :: (V a, V p)
instance (Value x, Value y) => Value (x:::y) where value  = V (x * y) where (V x, V y) = (value, value) :: (V x, V y)

-- | Compares integer /a/ to integer /b/.
--
--   Returns:
--
--   * 'LT' if (/a/ \< /b/)
--
--   * 'GT' if (/a/  > /b/)
--
--   * 'EQ' if (/a/  = /b/).
--
class                     Compare    a     b   c | a b -> c
instance                  Compare    Z     Z  EQ
instance                  Compare    Z  (S b) LT
instance                  Compare (S a)    Z  GT
instance Compare a b c => Compare (S a) (S b)  z

-- | Finds the smaller of two integers /a/ and /b/.
class                 Min    a     b     c | a b -> c
instance              Min    Z     Z     Z
instance              Min    Z  (S b)    Z
instance              Min (S a)    Z     Z
instance Min a b c => Min (S a) (S b) (S c)

-- | Finds the greater of two integers /a/ and /b/.
class                 Max    a     b     c | a b -> c
instance              Max    Z     Z     Z
instance              Max    Z  (S b) (S b)
instance              Max (S a)    Z  (S a)
instance Max a b c => Max (S a) (S b) (S c)

-- | Normalises product /x/ by removing all factors with a zero exponent.
class                     Normalise                x                 y | x -> y
instance                  Normalise                N                 N
instance Normalise y z => Normalise (a:^:(S x) ::: y) (a:^:(S x) ::: z)
instance Normalise y z => Normalise (a:^:   Z  ::: y)                z

-- | Finds the greatest common divisor of product /x/ and product /y/.
class                            GCD x y z | x y -> z
instance (C x y c, G c x y z) => GCD x y z

-- | Finds the least common multiple of product /x/ and product /y/.
class                            LCM x y z | x y -> z
instance (C x y c, L c x y z) => LCM x y z

-- | Divides product /x/ by product /y/ (assuming that /x/ is divisible by /y/).
class                                             Divide x y z | x y -> z
instance (C x y c, D c x y z', Normalise z' z) => Divide x y z

-- | Multiplies product /x/ with product /y/.
class                            Multiply x y z | x y -> z
instance (C x y c, M c x y z) => Multiply x y z

-- | Finds the greatest common divisor of product /x/ and product /y/, where
--   the heads /a/ and /b/ of products /x/ and /y/ have relative ordering /c/.
class                                                             G  c          x           y           z | c x y -> z
instance                                                          G EQ          N           N           N
instance                                                          G LT          N  (b:^:q:::y)          N
instance                                                          G GT (a:^:p:::x)          N           N
instance (C  x          y  c, G c  x          y  z, Min p q r) => G EQ (a:^:p:::x) (a:^:q:::y) (a:^:r:::z)
instance (C  x (b:^:q:::y) c, G c  x (b:^:q:::y) z           ) => G LT (a:^:p:::x) (b:^:q:::y)          z
instance (C (a:^:p:::x) y  c, G c (a:^:p:::x) y  z           ) => G GT (a:^:p:::x) (b:^:q:::y)          z

-- | Finds the least common multiple of product /x/ and product /y/, where
--   the heads /a/ and /b/ of products /x/ and /y/ have relative ordering /c/.
class                                                             L  c          x           y           z | c x y -> z
instance                                                          L EQ          N           N           N
instance                                                          L LT          N  (b:^:q:::y) (b:^:q:::y)
instance                                                          L GT (a:^:p:::x)          N  (a:^:p:::x)
instance (C  x          y  c, L c  x          y  z, Max p q r) => L EQ (a:^:p:::x) (a:^:q:::y) (a:^:r:::z)
instance (C  x (b:^:q:::y) c, L c  x (b:^:q:::y) z           ) => L LT (a:^:p:::x) (b:^:q:::y) (a:^:p:::z)
instance (C (a:^:p:::x) y  c, L c (a:^:p:::x) y  z           ) => L GT (a:^:p:::x) (b:^:q:::y) (b:^:q:::z)

-- | Divides product /x/ by product /y/ (assuming that /x/ is divisible by /y/),
--   where the heads /a/ and /b/ of products /x/ and /y/ have relative ordering /c/.
class                                                             D  c          x           y           z | c x y -> z
instance                                                          D EQ          N           N           N
instance                                                          D LT          N  (b:^:q:::y) (b:^:q:::y)
instance                                                          D GT (a:^:p:::x)          N  (a:^:p:::x)
instance (C  x          y  c, D c  x          y  z, Sub p q r) => D EQ (a:^:p:::x) (a:^:q:::y) (a:^:r:::z)
instance (C  x (b:^:q:::y) c, D c  x (b:^:q:::y) z           ) => D LT (a:^:p:::x) (b:^:q:::y) (a:^:p:::z)
instance (C (a:^:p:::x) y  c, D c (a:^:p:::x) y  z           ) => D GT (a:^:p:::x) (b:^:q:::y) (b:^:q:::z)

-- | Multiplies product /x/ with product /y/, where the heads
--   /a/ and /b/ of products /x/ and /y/ have relative ordering /c/.
class                                                             M  c          x           y           z | c x y -> z
instance                                                          M EQ          N           N           N
instance                                                          M LT          N  (b:^:q:::y) (b:^:q:::y)
instance                                                          M GT (a:^:p:::x)          N  (a:^:p:::x)
instance (C  x          y  c, M c  x          y  z, Add p q r) => M EQ (a:^:p:::x) (a:^:q:::y) (a:^:r:::z)
instance (C  x (b:^:q:::y) c, M c  x (b:^:q:::y) z           ) => M LT (a:^:p:::x) (b:^:q:::y) (a:^:p:::z)
instance (C (a:^:p:::x) y  c, M c (a:^:p:::x) y  z           ) => M GT (a:^:p:::x) (b:^:q:::y) (b:^:q:::z)

-- | Compares the head (/a/ ^ /p/) of product /x/
--   with the head (/b/ ^ /q/) of product /y/.
--
--   Returns:
--
--   * 'LT' if (/a/ \< /b/)
--
--   * 'GT' if (/a/  > /b/)
--
--   * 'EQ' if (/a/  = /b/).
--
class                                   C              x               y   c | x y -> c
instance                                C              N               N  EQ
instance                                C              N  (   b :^:q:::y) LT
instance                                C (   a :^:p:::x)              N  GT
instance                                C (   Z :^:p:::x) (   Z :^:q:::y) EQ
instance                                C (   Z :^:p:::x) ((S b):^:q:::y) LT
instance                                C ((S a):^:p:::x) (   Z :^:q:::y) GT
instance C (a:^:p:::x) (b:^:q:::y) z => C ((S a):^:p:::x) ((S b):^:q:::y) z

