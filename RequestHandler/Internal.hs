
module RequestHandler.Internal where

import Data.List

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

data RequestType = Image |
                   ImagePage |
                   FolderPage
                   deriving (Eq, Show)

handleRequest :: State -> String -> String
handleRequest state path = undefined

requestType :: String -> RequestType
requestType path
  | isSuffixOf htmlExt path = case isFolderPath pathWithoutHtml of
                                True  -> FolderPage
                                False -> ImagePage
  | otherwise = Image
  where
    htmlExt = ".html"
    len = length path
    pathWithoutHtml = take (len - length htmlExt) path

isFolderPath :: String -> Bool
isFolderPath path = not $ isSuffixOf ".jpg" path
