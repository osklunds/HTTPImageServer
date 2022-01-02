{-# LANGUAGE NamedFieldPuns #-}

module RequestHandler
( makeState
, handleRequest
)
where

import Data.List
import System.FilePath
import Data.ByteString (ByteString, hGetContents)
import System.IO (withBinaryFile, IOMode(..))

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
-- - .html
--   - folder
--   - image
-- - .thumb
-- - .full

handleRequest :: State -> String -> IO ByteString
handleRequest state pathWithExt =
    case ext of
        ".html" -> undefined
        ".full" -> handleFullImageRequest state path
        ".thumb" -> undefined
        -- todo: handle unexpected
    where
        (path, ext) = splitExtension pathWithExt

handleFullImageRequest :: State -> String -> IO ByteString
handleFullImageRequest (State { imageRootPath }) path =
    withBinaryFile filePath ReadMode hGetContents
    where
        filePath = imageRootPath </> path