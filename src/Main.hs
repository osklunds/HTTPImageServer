{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header
import Network.Wai.Handler.Warp (run)

import Data.ByteString.Builder (lazyByteString, stringUtf8)
import Blaze.ByteString.Builder (fromByteString, copyByteString)
import System.FilePath (joinPath)
import Data.Text as T (pack, unpack, Text)
import qualified Data.ByteString as S
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html

import System.Directory

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import qualified Data.ByteString.UTF8 as BSU      -- from utf8-string
import qualified Data.Text.Encoding as TSE
import qualified Data.Text.Lazy.Encoding as TLE

import RequestHandler

main :: IO ()
main = mainWithArgs "test/Thumbs" "test/Thumbs"

mainWithArgs :: String -> String -> IO ()
mainWithArgs thumbRoot fullRoot = run 3000 (app thumbRoot fullRoot)

app :: String -> String -> Application
app thumbRoot fullRoot request respond = do
    let state = makeState thumbRoot fullRoot 
    let path = joinPath $ map T.unpack $ pathInfo request
    -- resp <- handleRequest state path
    -- let strResp = T.unpack resp :: String
    resp <- handleRequest state path
    -- print $ responseBS
    --let responseLBS = lazyByteString responseBS
    let httpHeaders = [(hCacheControl, "public"),
                        (hCacheControl, "max-age=86400")]
    respond $ responseBuilder status200 httpHeaders $ copyByteString $ resp
    --let html = renderHtmlBuilder $ toHtml resp
    -- respond $ responseBuilder status200 httpHeaders html

    -- respond $ responseBuilder status200 [] $ copyByteString strResp

    -- let str = "yäyää"
    -- putStrLn str
    -- let text = T.pack str
    -- let strAgain = T.unpack text
    -- putStrLn strAgain

    -- let bs = (BSU.fromString str)
    -- putStrLn $ BSU.toString bs

    -- respond $ responseBuilder status200 [] $ copyByteString bs