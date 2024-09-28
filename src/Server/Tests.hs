
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Tests where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent
import Control.Monad
import Network.HTTP.Client
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString
import Control.Exception as CE
import Data.List
import Data.ByteString.Lazy as LBS (putStr)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS8 
import System.IO.Temp
import System.FilePath
import System.Directory

import Server

prop_normalCases :: Property
prop_normalCases = monadicIO $ run $ do
    withSystemTempDirectory "normalCases" (\thumbDir -> do
        withSystemTempDirectory "normalCases" (\fullImageDir ->
            normalCases thumbDir fullImageDir))

normalCases :: FilePath -> FilePath -> IO ()
normalCases thumbDir fullImageDir = do
    -- Create directories and images
    forM_ [thumbDir, fullImageDir]
          (\dir -> do
                       createDirectory $ dir </> "level1_1"
                       createDirectory $ dir </> "level1_1" </> "level2_1"
                       createDirectory $ dir </> "level1_1" </> "level2_2"
                       createDirectory $ dir </> "level1_1" </> "level2_2" </>
                                                 "level3"
                       createDirectory $ dir </> "level1_1" </> "level2_2" </>
                                                 "level3" </> "level4"
                       createDirectory $ dir </> "level1_2"
                       createDirectory $ dir </> "level1_3"
                       -- TODO: Special chars
          )

    createDirectory $ thumbDir </> "onlyInThumbs"
    createDirectory $ fullImageDir </> "onlyInFull"

    writeFile (thumbDir </> "level1_img.jpg") "level1_img_thumb"
    writeFile (thumbDir </> "level2_img.jpg") "level2_img_thumb"
    writeFile (fullImageDir </> "level1_img.jpg") "level1_img_full"
    writeFile (fullImageDir </> "level2_img.jpg") "level2_img_full"
    
    -- Start the server
    serverThread <- forkIO $ mainWithArgs thumbDir fullImageDir 12346

    -- Send requests
    responseRootFolderPage <- request ""
    responseLevel11 <- request "/level1_1"
    LBS.putStr $ responseLevel11

    -- Stop the server
    killThread serverThread

    -- Check responses
    assertContainsStrings responseRootFolderPage [
        -- Top button
        "<div class=\"top_button\" height=\"30px\" >\n\
        \/\n\
        \\n\
        \</div>",

        -- Folder button 1
        -- Only check all details once to avoid overspecifying in case of changes
        "<div class=\"folder_button\" onclick=\"window.location='/level1_1'\">\n\
        \    /level1_1\n\
        \</div>",

        -- Folder button 2
        "folder_button",
        "window.location",
        "/level1_2",
        "/level1_2",

        -- Folder button 3
        "folder_button",
        "window.location",
        "/level1_3",
        "/level1_3",

        -- Folder button onlyInThumbs
        "folder_button",
        "window.location",
        "/onlyInThumbs",
        "/onlyInThumbs",

        -- level1_img
        -- Only check all details once to avoid overspecifying in case of changes
        "<link rel=\"preload\" href=\"/level1_img.jpg.thumb\" as=\"image\"/>\n\
        \<div class=\"column\">\n\
        \    <div class=\"image_container\" onclick=\"window.location='/level1_img.jpg.html';\">\n\
        \        <img class=\"image\" loading=\"lazy\" src=\"/level1_img.jpg.thumb\" width=\"100%\">\n\
        \    </div>\n\
        \</div>",

        -- level2_img
        "preload",
        "/level2_img.jpg.thumb",
        "image_container",
        "window.location",
        "/level2_img.jpg.html",
        "img class",
        "/level2_img.jpg.thumb"
        ]

assertContainsStrings :: ByteString -> [ByteString] -> IO ()
assertContainsStrings haystack needles = do
    -- First check each individual string for easier debugging
    forM_ needles (\needle -> if haystack =~ needle
                      then return ()
                      else do
                          throwIO (AssertionFailed (LBS8.unpack needle)))

    -- Then check all at once to verify the order
    let opts = defaultCompOpt{multiline = False}
    let regex = makeRegexOpts opts
                              defaultExecOpt
                              (LBS8.intercalate ".*" needles)

    if match regex haystack
       then return ()
       else throwIO (AssertionFailed ("Regex not found"))

request :: String -> IO ByteString
request path = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest $ "http://127.0.0.1:12346" ++ path
    response <- httpLbs request manager
    return $ responseBody response


return []
runTests = $quickCheckAll
