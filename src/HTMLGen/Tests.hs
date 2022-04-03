
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HTMLGen.Tests where

import Test.QuickCheck
import Data.Text as T

import HTMLGen


prop_test = T.length (T.replicate 500000 (genImagePage input)) > 0
    where
        folderUrl = "folderUrl"
        fullImageUrl = "fullImageUrl"
        leftImagePageUrl = Just "leftImagePageUrl"
        rightImagePageUrl = Just "rightImagePageUrl"
        preloadImageUrls = ["preloadImageUrl1"]
        input 2= ImagePageInfo { folderUrl
                              , fullImageUrl
                              , leftImagePageUrl
                              , rightImagePageUrl
                              , preloadImageUrls
                              }

return []
runTests = $quickCheckAll
