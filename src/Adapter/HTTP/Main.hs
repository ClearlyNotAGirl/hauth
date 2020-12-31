module Adapter.HTTP.Main where

import ClassyPrelude hiding (delete)
import Web.Scotty.Trans

main :: IO ()
main = scottyT 3000 id routes

routes :: (MonadIO m) => ScottyT LText m ()
routes = do
  get "/" $ text "Home!"
  get "/hello/:name" $ do
    name <- param "name"
    text $ "hello, " <> name
  post "/users" $ text "adding user"
  put "/users/:id" $ text "updating user"
  patch "/users/:id" $ text "partially updating user"
  delete "/users/:id" $ text "deleting user"

  get "/add/:p1/:p2" $ do
    p1 <- param "p1"
    p2 <- param "p2"
    let sum = p1 + p2 :: Int
    text $ concat ["Finishing adding. Result is ", fromStrict $ tshow sum]

  matchAny "/admin" $ text "I don't care about your HTTP verb"
  options (regex ".*") $ text "CORS usually use this"

  notFound $ text "404"