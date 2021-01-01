module Adapter.HTTP.Web.Main where

import Domain.Auth
import ClassyPrelude
import Web.Scotty.Trans
import Katip
import Network.Wai
import Adapter.HTTP.Common

main :: WebContext m => (m Response -> IO Response) -> IO Application
main runner = scottyAppT runner routes

routes :: WebContext m => ScottyT LText m ()
routes = do 
    get "/" $ do
        text "Hello from web!"
