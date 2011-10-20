{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Data.TypeLevel.Comparison where

-- | The result of a type-level comparison between /a/ and /b/ such that (/a/ = /b/).
data EQ

-- | The result of a type-level comparison between /a/ and /b/ such that (/a/ \< /b/).
data LT

-- | The result of a type-level comparison between /a/ and /b/ such that (/a/ > /b/).
data GT

