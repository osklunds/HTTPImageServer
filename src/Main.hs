
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

import Blaze.ByteString.Builder (fromByteString)
import System.FilePath (joinPath)
import Data.Text (unpack)

import RequestHandler

main :: IO ()
main = run 3000 app

app :: Application
app request respond = do
    let state = makeState "test/Thumbs" "test/Images"
    let path = joinPath $ map unpack $ pathInfo request
    responseBS <- handleRequest state path
    let responseLBS = fromByteString responseBS
    let httpHeaders = []
    respond $ responseBuilder status200 httpHeaders responseLBS
