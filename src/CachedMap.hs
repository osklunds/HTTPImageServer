{-# LANGUAGE NamedFieldPuns #-}

module CachedMap
( CachedMap
, new
, get
)
where

import Data.Map hiding (map)
import Prelude hiding (map, lookup)


newtype CachedMap k v = CachedMap { map :: Map k v }

new :: CachedMap k v
new = CachedMap { map = empty }

get :: Ord k => k -> IO v -> CachedMap k v -> IO ((CachedMap k v), v)
get key calcValue cachedMap@(CachedMap { map }) = do
    case lookup key map of
        (Just value) ->
            return (cachedMap, value)
        Nothing -> do
            value <- calcValue
            let newMap = insert key value map
            return (CachedMap { map = newMap}, value)
            