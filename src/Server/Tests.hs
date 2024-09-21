
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
import Data.ByteString.Lazy.Char8 as LBS8 
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
    
    serverThread <- forkIO $ mainWithArgs thumbDir fullImageDir 12346

    manager <- newManager defaultManagerSettings

    request <- parseRequest "http://127.0.0.1:12346"
    response <- httpLbs request manager
    killThread serverThread

    LBS.putStr $ responseBody response

    let resp = responseBody response
    assertContainsStrings resp [
        -- Top button
        "<div class=\"top_button\" height=\"30px\" >\n\
        \/\n\
        \\n\
        \</div>",

        -- Folder button 1
        "<div class=\"folder_button\" onclick=\"window.location='/level1_1'\">\n\
        \    /level1_1\n\
        \</div>",

        -- Folder button 2
        "<div class=\"folder_button\" onclick=\"window.location='/level1_2'\">\n\
        \    /level1_2\n\
        \</div>",

        -- Folder button 3
        "<div class=\"folder_button\" onclick=\"window.location='/level1_3'\">\n\
        \    /level1_3\n\
        \</div>",

        -- Folder button onlyInThumbs
        "<div class=\"folder_button\" onclick=\"window.location='/onlyInThumbs'\">\n\
        \    /onlyInThumbs\n\
        \</div>"
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


return []
runTests = $quickCheckAll
