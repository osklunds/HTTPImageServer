{-# LANGUAGE TemplateHaskell #-}

module HTMLGen
( genImagePage
, genFolderPage
)
where

import Data.FileEmbed
import qualified Data.Text as T

genImagePage :: String -> Maybe String -> String -> Maybe String -> String
genImagePage parent leftImg curImg rightImg = replaceList rawPage repList
    where
        backgroundImageLink = curImg ++ ".full"
        (buttonLeftCursor, buttonLeftOnclick) = genSideButton leftImg
        (buttonRightCursor, buttonRightOnclick) = genSideButton rightImg
        buttonTopOnclick = genOnclick parent

        rawPage = $(embedStringFile "html/image_page/image_page.html")
        repList = [("BACKGROUND_IMAGE_LINK", backgroundImageLink),
                   ("BUTTON_LEFT_CURSOR", buttonLeftCursor),
                   ("BUTTON_RIGHT_CURSOR", buttonRightCursor),
                   ("BUTTON_LEFT_ONCLICK", buttonLeftOnclick),
                   ("BUTTON_RIGHT_ONCLICK", buttonRightOnclick),
                   ("BUTTON_TOP_ONCLICK", buttonTopOnclick)]

genSideButton :: Maybe String -> (String, String)
genSideButton Nothing = ("", "")
genSideButton (Just leftImg) = (genCursorPointer, genOnclick leftImg)

genCursorPointer :: String
genCursorPointer = "cursor: pointer;"

genOnclick :: String -> String
genOnclick path = "onclick=\"window.location='" ++ path ++"'\";"

replaceList :: String -> [(String, String)] -> String
replaceList str repList = foldl f str repList
    where
        f acc (needle, replacement) = replace needle replacement acc

replace :: String -> String -> String -> String
replace needle replacement haystack =
    T.unpack $ T.replace (T.pack needle)
                         (T.pack replacement)
                         (T.pack haystack)

genFolderPage :: String ->
                 Maybe String ->
                 [String] ->
                 [String] ->
                 String
genFolderPage title parent folderPaths imagePaths =
    replaceList rawPage repList
    where
        (buttonTopCursor, buttonTopOnclick) = case parent of
            Nothing -> ("", "")
            (Just p) -> (genCursorPointer, genOnclick p)
        folderList = concatMap genFolderButton folderPaths
        thumbnailList = concatMap genThumbnailButton imagePaths

        rawPage = $(embedStringFile "html/folder_page/folder_page.html")
        repList = [("BUTTON_TOP_CURSOR", buttonTopCursor),
                   ("BUTTON_TOP_ONCLICK", buttonTopOnclick),
                   ("BUTTON_TOP_TEXT", title),
                   ("FOLDER_LIST", folderList),
                   ("THUMBNAIL_LIST", thumbnailList)]

genFolderButton :: String -> String
genFolderButton folderPath = replaceList rawPage repList
    where
        rawPage = $(embedStringFile "html/folder_page/folder_button.html")
        repList = [("FOLDER_BUTTON_TEXT", folderPath)]

genThumbnailButton :: String -> String
genThumbnailButton imgPath = replaceList rawPage repList
    where
        rawPage = $(embedStringFile "html/folder_page/thumbnail.html")
        repList = [("THUMBNAIL_LINK", imgPath ++ ".html"),
                   ("THUMBNAIL_IMG", imgPath ++ ".thumb")]
