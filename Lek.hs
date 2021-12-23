{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

import System.Directory
import Data.ByteString.Lazy
import Data.String

app :: Application
app _request respond = do
  paths <- listDirectory "Images"
  let str = Prelude.concat paths
  respond $ responseLBS status200 [("Content-Type", "text/plain")] $ fromString str

main = run 3000 app
