module Data.TypeLevel.Integer.Synonyms where

import Data.TypeLevel.Integer

type N1 = N  Z
type N2 = N N1
type N3 = N N2
type N4 = N N3
type N5 = N N4
type N6 = N N5
type N7 = N N6
type N8 = N N7
type N9 = N N8

type P1 = P  Z
type P2 = P P1
type P3 = P P2
type P4 = P P3
type P5 = P P4
type P6 = P P5
type P7 = P P6
type P8 = P P7
type P9 = P P8


