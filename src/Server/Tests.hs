
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

import Server

prop_normalCases :: Property
prop_normalCases = monadicIO $ run $ normalCases

normalCases :: IO ()
normalCases = do
    serverThread <- forkIO $ mainWithArgs "Test/Thumbs" "Test/Thumbs" 12346

    manager <- newManager defaultManagerSettings

    request <- parseRequest "http://127.0.0.1:12346"
    response <- httpLbs request manager
    killThread serverThread

    -- LBS.putStr $ responseBody response

    let resp = responseBody response
    assertContainsStrings resp
                          ["subfolder",
                           "img2",
                           "img3",
                           "subfolder2åäö",
                           "img3"]

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
