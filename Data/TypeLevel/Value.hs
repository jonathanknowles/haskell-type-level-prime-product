{-# LANGUAGE MultiParamTypeClasses  #-}

module Data.TypeLevel.Value
	( V (..)
	, Value (..)
	) where

-- | Converts type /t/ into a value of type /v/.
class Value t v where value :: V t v

-- | The result of converting type /t/ into a value of type /v/.
data V t v = V v
