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


type CachedMap k v = Map k (UTCTime, v)

new :: CachedMap k v
new = empty

get :: Ord k => k -> IO v -> CachedMap k v -> IO ((CachedMap k v), v)
get key calcValue map = do
    now <- getCurrentTime
    getWithNow now key calcValue map

getWithNow :: Ord k => UTCTime ->
                       k ->
                       IO v ->
                       CachedMap k v ->
                       IO ((CachedMap k v), v)
getWithNow now key calcValue map = do
    case lookup key map of
        (Just (timeOfCalc, value)) -> do
            let diff = diffUTCTime now timeOfCalc
            if diff >= maxAge
                then
                    calculateAndUpdate now key calcValue map
                else
                    return (map, value)
        Nothing ->
            calculateAndUpdate now key calcValue map

calculateAndUpdate :: Ord k => UTCTime ->
                               k ->
                               IO v ->
                               CachedMap k v ->
                               IO ((CachedMap k v), v)
calculateAndUpdate now key calcValue map = do
    value <- calcValue
    let newMap = insert key (now, value) map
    return (newMap, value)

maxAge :: NominalDiffTime
maxAge = 600
