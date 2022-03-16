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

import RequestHandler

main :: IO ()
main = mainWithArgs "test/Thumbs" "test/Thumbs"

mainWithArgs :: String -> String -> IO ()
mainWithArgs thumbRoot fullRoot = run 3000 (app thumbRoot fullRoot)

app :: String -> String -> Application
app thumbRoot fullRoot request respond = do
    let state = makeState thumbRoot fullRoot 
    let path = joinPath $ map T.unpack $ pathInfo request
    (page, pageType) <- handleRequest state path
    let httpHeaders = case pageType of
                                Image ->
                                    [(hCacheControl, "public"),
                                     (hCacheControl, "max-age=86400")]
                                Navigation ->
                                    [(hCacheControl, "no-store")]
    respond $ responseBuilder status200 httpHeaders $ copyByteString $ page