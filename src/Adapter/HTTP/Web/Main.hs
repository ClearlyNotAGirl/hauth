module Adapter.HTTP.Web.Main where

import           Adapter.HTTP.Common
import           ClassyPrelude
import           Domain.Auth.Types
import           Katip
import           Network.Wai
import           Web.Scotty.Trans

main :: WebContext m => (m Response -> IO Response) -> IO Application
main runner = scottyAppT runner routes

routes :: WebContext m => ScottyT LText m ()
routes = do
  get "/" $ do text "Hello from web!"
