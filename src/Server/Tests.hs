
{-# LANGUAGE TemplateHaskell #-}

module Server.Tests where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent
import Control.Monad
import Network.HTTP.Client
import Text.Regex.TDFA
import Control.Exception as CE
import Data.List
import Data.ByteString.Lazy.Char8 as LBS8 (unpack)

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

    let resp = responseBody response
    assertContainsStrings (LBS8.unpack resp)
                          ["img2",
                           "img3",
                           "img3"]

assertContainsStrings :: String -> [String] -> IO ()
assertContainsStrings haystack needles = do
    -- First check each individual string for easier debugging
    forM_ needles (\needle -> if (needle `isInfixOf` haystack)
                      then return ()
                      else do
                          let msg = "Could not find '" ++
                                    needle ++
                                    "' inside\n" ++
                                    haystack
                          throwIO (AssertionFailed msg))

    -- Then check all at once to verify the order
    let opts = defaultCompOpt{multiline = False}
    let regex = makeRegexOpts opts
                              defaultExecOpt
                              (intercalate ".*" needles)

    if match regex haystack
       then return ()
       else throwIO (AssertionFailed ("Regex not found in\n" ++ haystack))


return []
runTests = $quickCheckAll
