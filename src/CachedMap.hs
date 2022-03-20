{-# LANGUAGE NamedFieldPuns #-}

module CachedMap
( CachedMap
, new
, get
, getWithNow
, maxAge
)
where

import Data.Map hiding (map)
import Prelude hiding (map, lookup)
import Data.Time.Clock


newtype CachedMap k v = CachedMap { map :: Map k (UTCTime, v) } deriving (Eq)

new :: CachedMap k v
new = CachedMap { map = empty }

get :: Ord k => k -> IO v -> CachedMap k v -> IO ((CachedMap k v), v)
get key calcValue cachedMap = do
    now <- getCurrentTime
    getWithNow now key calcValue cachedMap

getWithNow :: Ord k => UTCTime ->
                       k ->
                       IO v ->
                       CachedMap k v ->
                       IO ((CachedMap k v), v)
getWithNow now key calcValue cachedMap@(CachedMap { map }) = do
    case lookup key map of
        (Just (timeOfCalc, value)) -> do
            let diff = diffUTCTime now timeOfCalc
            if diff >= maxAge
                then
                    calculateAndUpdate now key calcValue map
                else
                    return (cachedMap, value)
        Nothing ->
            calculateAndUpdate now key calcValue map

calculateAndUpdate :: Ord k => UTCTime ->
                               k ->
                               IO v ->
                               Map k (UTCTime, v) ->
                               IO ((CachedMap k v), v)
calculateAndUpdate now key calcValue map = do
    value <- calcValue
    let newMap = insert key (now, value) map
    return (CachedMap { map = newMap}, value)

maxAge :: NominalDiffTime
maxAge = 600
