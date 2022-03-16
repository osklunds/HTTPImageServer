
{-# LANGUAGE TemplateHaskell #-}

module CachedMap.Tests where

import CachedMap

import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_get :: Int -> Int -> Property
prop_get key value = monadicIO $ do
    let map1 = new
    (_map2, valueGotten) <- run $ get key (return value) map1
    assert $ value == valueGotten

prop_get_two_keys :: Int -> Int -> Int -> Int -> Property
prop_get_two_keys key1 key2 value1 value2 = key1 /= key2 ==> monadicIO $ do
    let map1 = new
    (map2, _value) <- run $ get key1 (return value1) map1
    (_map3, valueGotten) <- run $ get key2 (return value2) map2
    assert $ value2 == valueGotten

prop_get_get :: Int -> Int -> Int -> Property
prop_get_get key value1 value2 = value1 /= value2 ==> monadicIO $ do
    let map1 = new
    (map2, _value) <- run $ get key (return value1) map1
    (_map3, valueGotten) <- run $ get key (return value2) map2
    assert $ value1 == valueGotten



return []
runTests = $quickCheckAll