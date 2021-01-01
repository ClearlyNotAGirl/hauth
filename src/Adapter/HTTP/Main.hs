module Adapter.HTTP.Main where

import ClassyPrelude
import Network.Wai
import Web.Scotty
import Domain.Auth
import Adapter.HTTP.Common
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Vhost
import qualified Adapter.HTTP.API.Main as API
import qualified Adapter.HTTP.Web.Main as Web
import Web.Scotty.Trans

-- type WebContext m = (MonadIO m, KatipContext m, AuthRepo m, EmailVerificationNotifier m, SessionRepo m)

main :: WebContext m => Int -> (m Response -> IO Response) -> IO ()
main port runner = do
    web <- Web.main runner
    api <- API.main runner
    run port $ vhost [(pathBeginsWith "api",api)] web
    where
        pathBeginsWith path req = headMay (pathInfo req) == Just path