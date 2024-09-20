
{-# LANGUAGE OverloadedStrings #-}

module Server
(
    main,
    mainWithArgs
)
where

import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Wai.Handler.Warp

import Blaze.ByteString.Builder
import Data.Text (unpack)
import System.FilePath
import Control.Concurrent.MVar
import System.Environment

import CachedMap
import RequestHandler

main :: IO ()
main = do
    args <- getArgs
    let [thumbRoot, fullRoot, portString] = args
    putStrLn $ "thumbail path: " ++ thumbRoot
    putStrLn $ "full images path: " ++ fullRoot
    putStrLn $ "port: " ++ portString

    let port = read portString

    mainWithArgs thumbRoot fullRoot port

mainTest :: IO ()
mainTest = mainWithArgs "test/Thumbs" "test/Thumbs" 3000

mainWithArgs :: String -> String -> Int -> IO ()
mainWithArgs thumbRoot fullRoot port = do
    cache <- newMVar new
    run port (app thumbRoot fullRoot cache)

app :: String -> String -> MVar Cache -> Application
app thumbRoot fullRoot cache request respond = do
    let state = makeState thumbRoot fullRoot cache
    let path = joinPath $ map unpack $ pathInfo request
    result <- handleRequest state path

    case result of
        Just (page, pageType) -> do
            let httpHeaders = case pageType of
                                    Image ->
                                        [(hCacheControl, "public, max-age=31536000")]
                                    Navigation ->
                                        [(hCacheControl, "public, max-age=600")]
            respond $ responseBuilder status200 httpHeaders $ copyByteString $ page
        Nothing ->
            respond $ responseBuilder status404 [] ""
