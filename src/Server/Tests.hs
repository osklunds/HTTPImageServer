
{-# LANGUAGE TemplateHaskell #-}

module Server.Tests where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Concurrent
import Network.HTTP.Client

import Server

prop_hej :: Property
prop_hej = monadicIO $ do
   run $ yo

yo :: IO ()
yo = do
    serverThread <- forkIO $ mainWithArgs "Test/Thumbs" "Test/Thumbs" 12346

    manager <- newManager defaultManagerSettings

    request <- parseRequest "http://127.0.0.1:12346"
    response <- httpLbs request manager

    _ <- killThread serverThread

    print $ responseBody response
    
    return ()


return []
runTests = $quickCheckAll
