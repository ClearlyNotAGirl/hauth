module Main where

import qualified Adapter.RabbitMQ.Auth as MQAuth
import Lib
import ClassyPrelude
import qualified Adapter.HTTP.Main as HTTP

main :: IO ()
main = withState $ \port le state@(_, _, mqState, _) -> do
  let runner = run le state
  MQAuth.init mqState runner
  HTTP.main port runner
