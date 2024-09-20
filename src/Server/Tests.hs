
{-# LANGUAGE TemplateHaskell #-}

module Server.Tests where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent
import Network.HTTP.Client
import Data.ByteString.Lazy as LBS

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

    LBS.putStr $ responseBody response


return []
runTests = $quickCheckAll
