
{-# LANGUAGE TemplateHaskell #-}

module CachedMap.Tests where

import CachedMap

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Time.Clock

prop_get :: Int -> Int -> Property
prop_get key value = monadicIO $ do
    -- Arrange
    let map1 = new

    -- Act
    (map2, valueGotten) <- run $ get key (return value) map1

    -- Assert
    assert $ map2 /= map1
    assert $ value == valueGotten

prop_get_two_keys :: Int -> Int -> Int -> Int -> Property
prop_get_two_keys key1 key2 value1 value2 = key1 /= key2 ==> monadicIO $ do
    -- Arrange
    let map1 = new
    (map2, _value) <- run $ get key1 (return value1) map1

    -- Act
    (map3, value2Gotten) <- run $ get key2 (return value2) map2

    -- Assert
    (_map4, value1Gotten) <- run $ get key1 (return value1) map3
    assert $ map3 /= map2
    assert $ value2 == value2Gotten
    assert $ value1 == value1Gotten

prop_get_cached :: Int -> Int -> Int -> Property
prop_get_cached key value1 value2 = value1 /= value2 ==> monadicIO $ do
    -- Arrange
    let map1 = new
    (map2, _value) <- run $ get key (return value1) map1
    
    -- Act
    (map3, valueGotten) <- run $ get key (return value2) map2

    -- Assert
    assert $ map3 == map2
    assert $ value1 == valueGotten

prop_get_cached_expired :: Int -> Int -> Int -> Property
prop_get_cached_expired key value1 value2 = value1 /= value2 ==> monadicIO $ do
    -- Arrange
    let map1 = new
    (map2, _value) <- run $ get key (return value1) map1
    
    -- Act
    now <- run $ getCurrentTime
    let newNow = addUTCTime (maxAge + 1) now
    (map3, valueGotten) <- run $ getWithNow newNow key (return value2) map2

    -- Assert
    assert $ map3 /= map2
    assert $ value2 == valueGotten

return []
runTests = $quickCheckAll