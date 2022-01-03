{-# LANGUAGE NamedFieldPuns #-}

module RequestHandler
( makeState
, handleRequest
)
where

import Data.List
import Control.Monad
import System.FilePath
import Data.ByteString (ByteString, hGetContents)
import System.IO (withBinaryFile, IOMode(..))
import System.Directory
import Data.String (fromString)

import HTMLGen

-- - For the current path
--     - If folder
--         - Create hedading with current path
--         - For each folder
--             - Create button to subfolder page
--         - For each image
--             - Create thumbnail with link
--     - If image
--         - Create fullscreen image
--         - Create left, up and down buttons

data State = State { thumbnailRootPath :: String
                   , imageRootPath :: String
                   } deriving (Show)

makeState :: String -> String -> State
makeState thumbnailRootPath imageRootPath =
    State { thumbnailRootPath, imageRootPath }

-- TODO: 4 request types
-- .html -> image page
-- .thumb
-- .full
-- no extension -> folder page

handleRequest :: State -> String -> IO ByteString
handleRequest state pathWithExt =
    case ext of
        "" -> handleFolderPageRequest state path
        ".html" -> handleImagePageRequest state path
        ".full" -> handleFullImageRequest state path
        ".thumb" -> handleThumbnailRequest state path
        -- todo: handle unexpected
    where
        (path, ext) = splitExtension pathWithExt

handleFolderPageRequest  :: State -> String -> IO ByteString
handleFolderPageRequest state path = do
    let title = "/" ++ path
    let parentPath = case path of
                        ""    -> Nothing
                        _else -> Just (takeDirectory path)
    let toThumbPath = pathToThumbnailPath state

    entries <- listDirectory $ toThumbPath path
    let entriesWithPath = map (path </>) entries
    folders <- filterM (isFolder . toThumbPath) entriesWithPath
    images <- filterM (isImage . toThumbPath) entriesWithPath

    let imgPathToThumbPath = (++ ".thumb")

    let html = genFolderPage title parentPath folders images imgPathToThumbPath
    return $ fromString html
        
pathToThumbnailPath :: State -> String -> String
pathToThumbnailPath (State { thumbnailRootPath }) path =
    thumbnailRootPath </> path

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

handleImagePageRequest  :: State -> String -> IO ByteString
handleImagePageRequest _state _path = undefined

handleFullImageRequest :: State -> String -> IO ByteString
handleFullImageRequest (State { imageRootPath }) path =
    withBinaryFile fullPath ReadMode hGetContents
    where
        fullPath = imageRootPath </> path

handleThumbnailRequest :: State -> String -> IO ByteString
handleThumbnailRequest (State { thumbnailRootPath }) path =
    withBinaryFile fullPath ReadMode hGetContents
    where
        fullPath = thumbnailRootPath </> path
