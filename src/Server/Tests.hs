
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
import System.Timeout

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

    writeFile (thumbDir </> "level1_img1.jpg") "content_of_level1_img1_thumb"
    writeFile (thumbDir </> "level1_img2.jpg") "content_of_level1_img2_thumb"
    writeFile (fullImageDir </> "level1_img1.jpg") "content_of_level1_img1_full"
    writeFile (fullImageDir </> "level1_img2.jpg") "content_of_level1_img2_full"
    
    -- Start the server
    serverThread <- forkIO $ do
        -- TODO: Timeout is a hack to avoid port re-us issue. Find the proper
        -- Way to kill this thread if parent thread dies
        timeout 1000000 $ mainWithArgs thumbDir fullImageDir 12345
        return ()

    -- Check responses folder pages
    assertResponseContainsStrings "" [
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

        -- level1_img1
        -- Only check all details once to avoid overspecifying in case of changes
        "<link rel=\"preload\" href=\"/level1_img1.jpg.thumb\" as=\"image\"/>\n\
        \<div class=\"column\">\n\
        \    <div class=\"image_container\" onclick=\"window.location='/level1_img1.jpg.html';\">\n\
        \        <img class=\"image\" loading=\"lazy\" src=\"/level1_img1.jpg.thumb\" width=\"100%\">\n\
        \    </div>\n\
        \</div>",

        -- level1_img2
        "preload",
        "/level1_img2.jpg.thumb",
        "image_container",
        "window.location",
        "/level1_img2.jpg.html",
        "img class",
        "/level1_img2.jpg.thumb"
        ]

    assertResponseContainsStrings "/level1_1" [
       -- Top button
       "<div class=\"top_button\" height=\"30px\" onclick=\\\"window.location='/.';\">\n\
       \/level1_1\n\
       \\n\
       \</div>",

       -- Folder button 2_1
       "folder_button",
       "window.location",
       "/level1_1/level2_1",
       "/level1_1/level2_1",

       -- Folder button 2_2
       "folder_button",
       "window.location",
       "/level1_1/level2_2",
       "/level1_1/level2_2"
       ]

    assertResponseContainsStrings "/level1_1/level2_1" [
       -- Top button
       "<div class=\"top_button\" height=\"30px\" onclick=\\\"window.location='/level1_1';\">\n\
       \/level1_1/level2_1\n\
       \\n\
       \</div>"
       ]

    assertResponseContainsStrings "/level1_1/level2_2" [
       -- Top button
       "<div class=\"top_button\" height=\"30px\" onclick=\\\"window.location='/level1_1';\">\n\
       \/level1_1/level2_2\n\
       \\n\
       \</div>",

       -- Folder button 3
       "folder_button",
       "window.location",
       "/level1_1/level2_2/level3",
       "/level1_1/level2_2/level3"
       ]

    assertResponseContainsStrings "/level1_1/level2_2/level3" [
       -- Top button
       "<div class=\"top_button\" height=\"30px\" onclick=\\\"window.location='/level1_1/level2_2';\">\n\
       \/level1_1/level2_2/level3\n\
       \\n\
       \</div>",

       -- Folder button 4
       "folder_button",
       "window.location",
       "/level1_1/level2_2/level3/level4",
       "/level1_1/level2_2/level3/level4"
       ]

    assertResponseContainsStrings "/onlyInThumbs" [
       -- Top button
       "<div class=\"top_button\" height=\"30px\" onclick=\\\"window.location='/.';\">\n\
       \/onlyInThumbs\n\
       \\n\
       \</div>"
       ]

    -- Check responses image pages
    assertResponseContainsStrings "/level1_img1.jpg.html" [
        -- The image itself
        "background-image: url\\(\"/./level1_img1.jpg.full\"\\);",

        -- Navigation buttons
        "<div class=\"left_button\" ></div>",
        "<div class=\"top_button\" onclick=\"window.location='/.';\"></div>",
        "<div class=\"right_button\" onclick=\"window.location='/./level1_img2.jpg.html';\"></div>",

        -- Preload of neighbor images
        "<link rel=\"preload\" href=\"/./level1_img1.jpg.full\" as=\"image\"/>",

        -- Preload of neighbor pages
        "<link rel=\"preload\" href=\"/./level1_img1.jpg.html\" as=\"image\"/>"
        ]

    assertResponseContainsStrings "/level1_img2.jpg.html" [
        -- The image itself
        "background-image: url\\(\"/./level1_img2.jpg.full\"\\);",

        -- Navigation buttons
        "<div class=\"left_button\" onclick=\"window.location='/./level1_img1.jpg.html';\"></div>",
        "<div class=\"top_button\" onclick=\"window.location='/.';\"></div>",
        "<div class=\"right_button\" ></div>",

        -- Preload of neighbor images
        "<link rel=\"preload\" href=\"/./level1_img2.jpg.full\" as=\"image\"/>",

        -- Preload of neighbor pages
        "<link rel=\"preload\" href=\"/./level1_img2.jpg.html\" as=\"image\"/>"
        ]


    -- TODO: Paths that don't exist
    -- TODO: Fail test if server crashes. If it returns, can kill parent thread
    -- TODO: Enough images to test caching of neighbors
    
    -- Stop the server
    killThread serverThread

assertResponseContainsStrings :: String -> [ByteString] -> IO ()
assertResponseContainsStrings path needles = do
    response <- request path
    LBS.putStr response
    assertContainsStrings response needles

request :: String -> IO ByteString
request path = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest $ "http://127.0.0.1:12345" ++ path
    response <- httpLbs request manager
    return $ responseBody response

assertContainsStrings :: ByteString -> [ByteString] -> IO ()
assertContainsStrings haystack needles = do
    -- TODO: regex-quote each needle
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


return []
runTests = $quickCheckAll
