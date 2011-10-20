{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Type-level encodings and operations for products of prime numbers.
module Data.TypeLevel.PrimeProduct.Dense where

infixr 0 :::

-- | A type-level representation of the integer /zero/.
data Z

-- | A type-level representation of a /positive integer/.
data S x

-- | A type-level representation of a /prime product/
--   encoded as a list of prime exponents [  /p/,   /q/,   /r/ ... ]
--   representing the prime product       [2^/p/, 3^/q/, 5^/r/ ... ].
data primeExponent ::: tail

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

-- | Finds the greatest common divisor of product /x/ and product /y/.
class                                  GCD x y z | x y -> z
instance (G x y z', Normalise z' z) => GCD x y z

-- | Finds the least common multiple of product /x/ and product /y/.
class                              LCM      x       y       z | x y -> z
instance                           LCM      N       N       N
instance                           LCM      N  (q:::y) (q:::y)
instance                           LCM (p:::x)      N  (p:::x)
instance (LCM x y z, Max p q r) => LCM (p:::x) (q:::y) (r:::z)

-- | Divides product /x/ by product /y/ (assuming that /x/ is divisible by /y/).
class                                  Divide x y z | x y -> z
instance (D x y z', Normalise z' z) => Divide x y z

-- | Multiplies product /x/ with product /y/.
class                                   Multiply      x       y       z | x y -> z
instance                                Multiply      N       N       N
instance                                Multiply      N  (q:::y) (q:::y)
instance                                Multiply (p:::x)      N  (p:::x)
instance (Multiply x y z, Add p q r) => Multiply (p:::x) (q:::y) (r:::z)

-- | Normalises product /x/. Removes all trailing zero exponents.
class                                                  Normalise x y | x -> y
instance (Reverse x a, Decapitate a b, Reverse b y) => Normalise x y

-- | Iteratively removes zero exponents from the head of product /x/.
class                      Decapitate          x           y | x -> y
instance                   Decapitate          N           N
instance Decapitate x y => Decapitate (   Z :::x)          y
instance                   Decapitate ((S a):::x) ((S a):::x)

-- | Reverses the list that encodes product /x/.
class               Reverse x y | x -> y
instance R x N y => Reverse x y

-- | Divides product /x/ by product /y/ (assuming that /x/ is divisible by /y/),
--   producing a result that may require normalisation with 'Normalise'.
class                            D      x       y       z | x y -> z
instance                         D      N       N       N
instance (D N y N           ) => D      N  (Z:::y)      N
instance                         D (p:::x)      N  (p:::x)
instance (D x y z, Sub p q r) => D (p:::x) (q:::y) (r:::z)

-- | Finds the greatest common divisor of product /x/ and product /y/.
--   producing a result that may require normalisation with 'Normalise'.
class                            G      x       y       z | x y -> z
instance                         G      N       N       N
instance                         G      N  (q:::y)      N
instance                         G (p:::x)      N       N
instance (G x y z, Min p q r) => G (p:::x) (q:::y) (r:::z)

-- | Reverses the list that encodes product /x/, using the accumulator /a/.
class                     R      x  a y | x a -> y
instance                  R      N  a a
instance R x (p:::a) z => R (p:::x) a z

