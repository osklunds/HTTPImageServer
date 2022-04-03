{-# LANGUAGE NamedFieldPuns #-}

module RequestHandler
( makeState
, handleRequest
, PageType(..)
, Cache
)
where

import Data.List
import Control.Monad
import System.FilePath
import Data.ByteString (ByteString, hGetContents)
import System.IO (withBinaryFile, IOMode(..))
import System.Directory
import qualified Data.Text as T
import Data.Text.Encoding
import Control.Concurrent.MVar

import HTMLGen
import CachedMap

type Cache = CachedMap FilePath [FilePath]

data State = State { thumbnailRootPath :: FilePath
                   , imageRootPath :: FilePath
                   , cache :: MVar Cache
                   }

makeState :: String -> String -> MVar Cache -> State
makeState thumbnailRootPath imageRootPath cache =
    State { thumbnailRootPath, imageRootPath, cache }

{-
# Overview page

ReqURL:
folder/subfolder

Generated URLs:
folder
folder/subfolder/anotherfolder
folder/subfolder name1.png.jpg.html
folder/subfolder name1.png.jpg.thumb
folder/subfolder name2.png.jpg.html
folder/subfolder name2.png.jpg.thumb
folder/subfolder name3.png.jpg.html
folder/subfolder name3.png.jpg.thumb

# Img page

ReqURL:
folder/subfolder name2.png.jpg.html

Generated URLs:
folder/subfolder
folder/subfolder name1.png.jpg.html
folder/subfolder name2.png.jpg.full
folder/subfolder name3.png.jpg.html

# Thumbnail

ReqURL:
folder/subfolder name1.png.jpg.thumb

Path:
folder/subfolder name1.png.jpg

# Full image

ReqURL:
folder/subfolder name1.png.jpg.full

Path:
folder/subfolder/name1.png
-}

data PageType = Navigation | Image

handleRequest :: State -> String -> IO (ByteString, PageType)
handleRequest state pathWithExt = do
    let (path, ext) = splitExtension pathWithExt

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
genFolderPageInfo (State { thumbnailRootPath, cache }) path = do
    let title = T.pack $ "/" ++ path
    let parentUrl = case path of
                        ""    -> Nothing
                        _else -> Just $ T.pack $ "/" ++ takeDirectory path
    let addThumbPath = (thumbnailRootPath </>)

    entries <- listDirectoryCached cache $ addThumbPath path
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
genImagePageInfo (State { thumbnailRootPath, cache }) url = do
    -- folderUrl
    let folderPath = takeDirectory url 
    let folderUrl = T.pack $ "/" ++ folderPath

    -- fullImageUrl
    let addThumbPath = (thumbnailRootPath </>)

    entries <- listDirectoryCached cache $ addThumbPath folderPath
    let entriesWithPath = map (folderPath </>) entries
    images <- filterM (isImage . addThumbPath) entriesWithPath
    let sortedImages = sort images
    let currentImage = folderPath </> takeFileName url
    let (Just index) = findIndex (==currentImage) sortedImages
    
    let fullImageUrlFromIndex = urlFromIndex sortedImages ".full"

    let fullImageUrl = fullImageUrlFromIndex index

    -- preloadImageUrls
    let spread = 5
    let maxIndex = length sortedImages - 1
    let indexes = [cap 0 maxIndex i | i <- [index-spread..index+spread]]
    let preloadImageUrls = map fullImageUrlFromIndex indexes

    -- left/rightImagePageUrl
    let pageUrlFromIndex = urlFromIndex sortedImages ".html"

    let leftImagePageUrl = case index == 0 of
                                True -> Nothing
                                False -> Just $ pageUrlFromIndex $ index - 1
    let rightImagePageUrl = case index == maxIndex of
                                True -> Nothing
                                False -> Just $ pageUrlFromIndex $ index + 1

    return $ ImagePageInfo { folderUrl
                           , fullImageUrl
                           , leftImagePageUrl
                           , rightImagePageUrl
                           , preloadImageUrls }

urlFromIndex :: [FilePath] -> FilePath -> (Int -> T.Text)
urlFromIndex sortedImages extension i = T.pack $ "/" ++ imageName ++ extension
    where
        imageName = sortedImages !! i

cap :: Int -> Int -> Int -> Int
cap mini maxi val = min (max mini val) maxi

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

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

listDirectoryCached :: MVar Cache -> FilePath -> IO [FilePath]
listDirectoryCached cacheMVar filePath = do
    cache <- takeMVar cacheMVar
    let doListDirectory = do
                            putStrLn $ "real list for: " ++ filePath
                            listDirectory filePath
    (newCache, entries) <- get filePath doListDirectory cache
    putMVar cacheMVar newCache
    return entries
