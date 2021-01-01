module Adapter.HTTP.API.Main where

import Domain.Auth.Types
import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Adapter.HTTP.API.Auth as AuthAPI
import Adapter.HTTP.Common
import Katip
import Network.Wai
import Network.Wai.Middleware.Gzip

main :: WebContext m  => (m Response -> IO Response) -> IO Application
main runner = scottyAppT runner routes

routes :: WebContext m  => ScottyT LText m ()
routes = do
  middleware $ gzip $ def {gzipFiles = GzipCompress}
  AuthAPI.routes

  notFound $ do
    status status404
    json $ errorResponse ("NotFound" :: Text)

  defaultHandler $ \e -> do
    lift $ $(logTM) ErrorS $ "Unhandled error: " <> ls (showError e)
    status status500
    json ("InternalServerError"::Text)