
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

import Blaze.ByteString.Builder (fromByteString)
import System.FilePath (joinPath)
import Data.Text (unpack)

import RequestHandler

main :: IO ()
main = mainWithArgs "test/Thumbs" "test/Images"

mainWithArgs :: String -> String -> IO ()
mainWithArgs thumbRoot fullRoot = run 3000 (app thumbRoot fullRoot)

app :: String -> String -> Application
app thumbRoot fullRoot request respond = do
    let state = makeState thumbRoot fullRoot 
    let path = joinPath $ map unpack $ pathInfo request
    responseBS <- handleRequest state path
    let responseLBS = fromByteString responseBS
    let httpHeaders = []
    respond $ responseBuilder status200 httpHeaders responseLBS
