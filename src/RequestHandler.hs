{-# LANGUAGE NamedFieldPuns #-}

module RequestHandler
( makeState
, handleRequest
, PageType(..)
)
where

import Data.List
import Control.Monad
import System.FilePath
import Data.ByteString (ByteString, hGetContents, unpack)
import System.IO (withBinaryFile, IOMode(..))
import System.Directory
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Encoding

import HTMLGen

data State = State { thumbnailRootPath :: String
                   , imageRootPath :: String
                   } deriving (Show)

makeState :: String -> String -> State
makeState thumbnailRootPath imageRootPath =
    State { thumbnailRootPath, imageRootPath }

{-
# Overview page

ReqURL:
folder/subfolder

Generated URLs:
folder
folder/subfolder/anotherfolder
folder/subfolder   <DATE> name1.png.jpg.html
folder/subfolder   <DATE> name1.png.jpg.thumb
folder/subfolder   <DATE> name2.png.jpg.html
folder/subfolder   <DATE> name2.png.jpg.thumb
folder/subfolder   <DATE> name3.png.jpg.html
folder/subfolder   <DATE> name3.png.jpg.thumb

# Img page

ReqURL:
folder/subfolder   <DATE> name2.png.jpg.html

Generated URLs:
folder/subfolder
folder/subfolder   <DATE> name1.png.jpg.html
folder/subfolder   <DATE> name2.png.jpg.full
folder/subfolder   <DATE> name3.png.jpg.html

# Thumbnail

ReqURL:
folder/subfolder   <DATE> name1.png.jpg.thumb

Path:
folder/subfolder   <DATE> name1.png.jpg

# Full image

ReqURL:
folder/subfolder   <DATE> name1.png.jpg.full

Path:
folder/subfolder/name1.png
-}

data PageType = Navigation | Image

handleRequest :: State -> String -> IO (ByteString, PageType)
handleRequest state pathWithExt = do
    let (path, ext) = splitExtension pathWithExt
    print $ "Request: " ++ pathWithExt
    let (handler, pageType) = case ext of
                                "" -> (handleFolderPageRequest, Navigation)
                                ".html" -> (handleImagePageRequest, Navigation)
                                ".full" -> (handleFullImageRequest, Image)
                                ".thumb" -> (handleThumbnailRequest, Image)
    page <- handler state path
    return (page, pageType)

--------------------------------------------------------------------------------
-- Folder page request
--------------------------------------------------------------------------------

handleFolderPageRequest  :: State -> String -> IO ByteString
handleFolderPageRequest state path = do
    info <- genFolderPageInfo state path
    let html = genFolderPage info
    return $ encodeUtf8 html

genFolderPageInfo :: State -> String -> IO FolderPageInfo
genFolderPageInfo (State { thumbnailRootPath }) path = do
    let title = T.pack $ "/" ++ path
    let parentUrl = case path of
                        ""    -> Nothing
                        _else -> Just $ T.pack $ "/" ++ takeDirectory path
    let addThumbPath = (thumbnailRootPath </>)

    entries <- listDirectory $ addThumbPath path
    let sortedEntries = sort entries
    let entriesWithPath = map (path </>) sortedEntries
    folderUrlsStr <- filterM (isFolder . addThumbPath) entriesWithPath
    let folderUrls = map T.pack folderUrlsStr

    images <- filterM (isImage . addThumbPath) entriesWithPath
    
    let imageUrlPairs = map genImageUrlPair images

    return $ FolderPageInfo { title, parentUrl, folderUrls, imageUrlPairs }

genImageUrlPair :: String -> ImageUrlPair
genImageUrlPair url = ImageUrlPair { imagePageUrl, thumbnailUrl }
    where
        imagePageUrl = T.pack $ "/" ++ url ++ ".html"
        thumbnailUrl = T.pack $ "/" ++ url ++ ".thumb"

isFolder :: FilePath -> IO Bool
isFolder = doesDirectoryExist

isImage :: FilePath -> IO Bool
isImage imgPath = do
    isFile <- doesFileExist imgPath
    if isFile
        then do
            let ext = takeExtension imgPath
            return $ ext `elem` [".jpg", ".jpeg", ".png"]
        else
            return False

--------------------------------------------------------------------------------
-- Image page request
--------------------------------------------------------------------------------

handleImagePageRequest  :: State -> FilePath -> IO ByteString
handleImagePageRequest state url = do
    info <- genImagePageInfo state url
    let html = genImagePage info
    return $ encodeUtf8 html

genImagePageInfo :: State -> FilePath -> IO ImagePageInfo
genImagePageInfo (State { thumbnailRootPath }) url = do
    let folderPath = takeDirectory url 
    let folderUrl = T.pack $ "/" ++ folderPath
    let fullImageUrl = T.pack $ "/" ++ url ++ ".full"

    let addThumbPath = (thumbnailRootPath </>)

    entries <- listDirectory $ addThumbPath folderPath
    let entriesWithPath = map (folderPath </>) entries
    images <- filterM (isImage . addThumbPath) entriesWithPath
    let sortedImages = sort images
    let currentImage = folderPath </> takeFileName url
    let (Just index) = findIndex (==currentImage) sortedImages
    let pageUrlFromIndex i = let imageName = sortedImages !! i
                             in "/" ++ imageName ++ ".html"

    let leftImagePageUrl = case index == 0 of
                                True -> Nothing
                                False -> Just $ T.pack $ pageUrlFromIndex $ index - 1
    let rightImagePageUrl = case index == (length sortedImages - 1) of
                                True -> Nothing
                                False -> Just $ T.pack $ pageUrlFromIndex $ index + 1

    {-
    
    1. from image url, find folder
    2. list all entries and filter images
    3. sort images
    4. find index of current image
    5. -1 and +1 are left and right names

    -}

    return $ ImagePageInfo { folderUrl
                           , fullImageUrl
                           , leftImagePageUrl
                           , rightImagePageUrl }

--------------------------------------------------------------------------------
-- Full image request
--------------------------------------------------------------------------------

handleFullImageRequest :: State -> String -> IO ByteString
handleFullImageRequest (State { imageRootPath }) path =
    withBinaryFile fullPath ReadMode hGetContents
    where
        fullPath = imageRootPath </> path

--------------------------------------------------------------------------------
-- Thumbnail request
--------------------------------------------------------------------------------

handleThumbnailRequest :: State -> String -> IO ByteString
handleThumbnailRequest (State { thumbnailRootPath }) path =
    withBinaryFile fullPath ReadMode hGetContents
    where
        fullPath = thumbnailRootPath </> path
