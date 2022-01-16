{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module HTMLGen
( genImagePage
, ImagePageInfo(..)
, genFolderPage
, FolderPageInfo(..)
, ImageUrlPair(..)
)
where

import Data.FileEmbed
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Image page
--------------------------------------------------------------------------------

data ImagePageInfo = ImagePageInfo { folderUrl :: String
                                   , fullImageUrl :: String
                                   , leftImagePageUrl :: Maybe String
                                   , rightImagePageUrl :: Maybe String
                                   }

genImagePage :: ImagePageInfo -> String
genImagePage info = replaceList rawPage repList
        where
            (ImagePageInfo { folderUrl
                           , fullImageUrl
                           , leftImagePageUrl
                           , rightImagePageUrl }) = info

            backgroundImageLink = fullImageUrl
            (leftButtonCursor,
             leftButtonOnclick) = genSideButton leftImagePageUrl
            (rightButtonCursor,
             rightButtonOnclick) = genSideButton rightImagePageUrl
            topButtonOnclick = genOnclick folderUrl

            rawPage = $(embedStringFile "html/image_page/image_page.html")
            repList = [("BACKGROUND_IMAGE_LINK", backgroundImageLink),
                       ("LEFT_BUTTON_CURSOR", leftButtonCursor),
                       ("RIGHT_BUTTON_CURSOR", rightButtonCursor),
                       ("LEFT_BUTTON_ONCLICK", leftButtonOnclick),
                       ("RIGHT_BUTTON_ONCLICK", rightButtonOnclick),
                       ("TOP_BUTTON_ONCLICK", topButtonOnclick)]

genSideButton :: Maybe String -> (String, String)
genSideButton Nothing = ("", "")
genSideButton (Just imagePage) = (genCursorPointer, genOnclick imagePage)

genCursorPointer :: String
genCursorPointer = "cursor: pointer;"

genOnclick :: String -> String
genOnclick url = "onclick=\"window.location='" ++ url ++"';\""

--------------------------------------------------------------------------------
-- Folder page
--------------------------------------------------------------------------------

data FolderPageInfo = FolderPageInfo { title :: String
                                     , parentUrl :: Maybe String
                                     , folderUrls :: [String]
                                     , imageUrlPairs :: [ImageUrlPair]
                                     }

data ImageUrlPair = ImageUrlPair { imagePageUrl :: String
                                 , thumbnailUrl :: String
                                 }

genFolderPage :: FolderPageInfo -> String
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

genFolderButton :: String -> String
genFolderButton folderPath = replaceList rawPage repList
    where
        rawPage = $(embedStringFile "html/folder_page/folder_button.html")
        repList = [("FOLDER_BUTTON_TEXT", folderPath)]

genThumbnailButton :: ImageUrlPair -> String
genThumbnailButton imageUrlPair = replaceList rawPage repList
    where
        (ImageUrlPair {imagePageUrl, thumbnailUrl}) = imageUrlPair

        rawPage = $(embedStringFile "html/folder_page/thumbnail.html")
        repList = [("THUMBNAIL_LINK", imagePageUrl),
                   ("THUMBNAIL_IMG", thumbnailUrl)]

--------------------------------------------------------------------------------
-- Common
--------------------------------------------------------------------------------

replaceList :: String -> [(String, String)] -> String
replaceList str repList = foldl f str repList
        where
                f acc (needle, replacement) = replace needle replacement acc

replace :: String -> String -> String -> String
replace needle replacement haystack =
        T.unpack $ T.replace (T.pack needle)
                                                 (T.pack replacement)
                                                 (T.pack haystack)