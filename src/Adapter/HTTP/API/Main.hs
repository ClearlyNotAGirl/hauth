module Adapter.HTTP.API.Main where

import qualified Adapter.HTTP.API.Auth       as AuthAPI
import           Adapter.HTTP.Common
import           ClassyPrelude
import           Domain.Auth.Types
import           Katip
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Middleware.Gzip
import           Web.Scotty.Trans

main :: WebContext m => (m Response -> IO Response) -> IO Application
main runner = scottyAppT runner routes

routes :: WebContext m => ScottyT LText m ()
routes = do
  middleware $ gzip $ def {gzipFiles = GzipCompress}
  AuthAPI.routes
  notFound $ do
    status status404
    json $ errorResponse ("NotFound" :: Text)
  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
    status status500
    json ("InternalServerError" :: Text)
