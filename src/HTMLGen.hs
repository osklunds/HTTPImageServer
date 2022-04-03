{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module HTMLGen
( genImagePage
, ImagePageInfo(..)
, genFolderPage
, FolderPageInfo(..)
, ImageUrlPair(..)
)
where

import Prelude hiding (concatMap)
import Data.FileEmbed
import qualified Data.Text as T
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Image page
--------------------------------------------------------------------------------

data ImagePageInfo = ImagePageInfo { folderUrl :: Text
                                   , fullImageUrl :: Text
                                   , leftImagePageUrl :: Maybe Text
                                   , rightImagePageUrl :: Maybe Text
                                   , preloadImageUrls :: [Text]
                                   }

genImagePage :: ImagePageInfo -> Text
genImagePage info = replaceList rawPage repList
    where
        (ImagePageInfo { folderUrl
                       , fullImageUrl
                       , leftImagePageUrl
                       , rightImagePageUrl
                       , preloadImageUrls }) = info

        backgroundImageLink = fullImageUrl
        (leftButtonCursor, leftButtonOnclick) = genSideButton leftImagePageUrl
        (rightButtonCursor, rightButtonOnclick) = genSideButton rightImagePageUrl
        topButtonOnclick = genOnclick folderUrl
        preloadImages = concatMap genPreloadImage preloadImageUrls

        rawPage = $(embedStringFile "html/image_page/image_page.html")
        repList = [("BACKGROUND_IMAGE_LINK", backgroundImageLink),
                   ("LEFT_BUTTON_CURSOR", leftButtonCursor),
                   ("RIGHT_BUTTON_CURSOR", rightButtonCursor),
                   ("LEFT_BUTTON_ONCLICK", leftButtonOnclick),
                   ("RIGHT_BUTTON_ONCLICK", rightButtonOnclick),
                   ("TOP_BUTTON_ONCLICK", topButtonOnclick),
                   ("PRELOAD_IMAGES", preloadImages)]

genSideButton :: Maybe Text -> (Text, Text)
genSideButton Nothing = ("", "")
genSideButton (Just imagePage) = (genCursorPointer, genOnclick imagePage)

genCursorPointer :: Text
genCursorPointer = "cursor: pointer;"

genOnclick :: Text -> Text
genOnclick url = T.concat ["onclick=\"window.location='", url, "';\""]

genPreloadImage :: Text -> Text
genPreloadImage imageUrl = replaceList rawPage repList
    where
        rawPage = $(embedStringFile "html/image_page/preload_image.html")
        repList = [("PRELOAD_IMG", imageUrl)]

--------------------------------------------------------------------------------
-- Folder page
--------------------------------------------------------------------------------

data FolderPageInfo = FolderPageInfo { title :: Text
                                     , parentUrl :: Maybe Text
                                     , folderUrls :: [Text]
                                     , imageUrlPairs :: [ImageUrlPair]
                                     }

data ImageUrlPair = ImageUrlPair { imagePageUrl :: Text
                                 , thumbnailUrl :: Text
                                 }
                                 deriving (Show)

genFolderPage :: FolderPageInfo -> Text
genFolderPage info =
    replaceList rawPage repList
    where
        (FolderPageInfo { title
                        , parentUrl
                        , folderUrls
                        , imageUrlPairs }) = info

        (topButtonCursor, topButtonOnclick) = case parentUrl of
            Nothing -> ("", "")
            (Just url) -> (genCursorPointer, genOnclick url)
        folderList = concatMap genFolderButton folderUrls
        thumbnailList = concatMap genThumbnailButton imageUrlPairs

        rawPage = $(embedStringFile "html/folder_page/folder_page.html")
        repList = [("TOP_BUTTON_CURSOR", topButtonCursor),
                   ("TOP_BUTTON_ONCLICK", topButtonOnclick),
                   ("TOP_BUTTON_TEXT", title),
                   ("FOLDER_LIST", folderList),
                   ("THUMBNAIL_LIST", thumbnailList)]

genFolderButton :: Text -> Text
genFolderButton folderPath = replaceList rawPage repList
    where
        rawPage = $(embedStringFile "html/folder_page/folder_button.html")
        repList = [("FOLDER_BUTTON_TEXT", folderPath)]

genThumbnailButton :: ImageUrlPair -> Text
genThumbnailButton imageUrlPair = replaceList rawPage repList
    where
        (ImageUrlPair {imagePageUrl, thumbnailUrl}) = imageUrlPair

        rawPage = $(embedStringFile "html/folder_page/thumbnail.html")
        repList = [("THUMBNAIL_LINK", imagePageUrl),
                   ("THUMBNAIL_IMG", thumbnailUrl)]

--------------------------------------------------------------------------------
-- Common
--------------------------------------------------------------------------------

replaceList :: Text -> [(Text, Text)] -> Text
replaceList str repList = foldl f str repList
    where
        f acc (needle, replacement) = T.replace needle replacement acc

concatMap :: (a -> Text) -> [a] -> Text
concatMap f ts = T.concat $ map f ts