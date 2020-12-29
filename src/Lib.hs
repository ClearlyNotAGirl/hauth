module Lib
where

import ClassyPrelude
import Domain.Auth

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Throwaway code

instance AuthRepo IO where
  addAuth (Auth email pass) = do
    putStrLn $ "Adding auth: " <> rawEmail email
    return $ Right "fake verification code"

instance EmailVerificationNotifier IO where
  notifyEmailVerification email vcode =
    putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode