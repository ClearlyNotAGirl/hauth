module Adapter.HTTP.API.Auth where

import           Adapter.HTTP.API.Common
import           Adapter.HTTP.Common
import           ClassyPrelude
import           Data.Aeson                hiding (json, (.:))
import           Domain.Auth.Types
import           Katip
import           Network.HTTP.Types.Status
import           Text.Digestive.Form       ((.:))
import qualified Text.Digestive.Form       as DF
import           Web.Scotty.Trans

authForm :: (Monad m) => DF.Form [Text] m Auth
authForm = Auth <$> "email" .: emailForm <*> "password" .: passwordForm
  where
    emailForm = DF.validate (toResult . mkEmail) (DF.text Nothing)
    passwordForm = DF.validate (toResult . mkPassword) (DF.text Nothing)

verifyEmailForm :: (Monad m) => DF.Form [Text] m VerificationCode
verifyEmailForm = DF.text Nothing

routes ::
     (ScottyError e, MonadIO m, KatipContext m, AuthService m) => ScottyT e m ()
routes = do
  post "/api/auth/register" $ do
    input <- parseAndValidateJSON authForm
    domainResult <- lift $ register input
    case domainResult of
      Left RegistrationErrorEmailTaken -> do
        status status400
        json ("EmailTaken" :: Text)
      Right _ -> return ()
  post "/api/auth/verifyEmail" $ do
    input <- parseAndValidateJSON verifyEmailForm
    domainResult <- lift $ verifyEmail input
    case domainResult of
      Left EmailVerificationErrorInvalidCode -> do
        status status400
        json ("Invalid code" :: Text)
      Right _ -> return ()
  post "/api/auth/login" $ do
    input <- parseAndValidateJSON authForm
    domainResult <- lift $ login input
    case domainResult of
      Left LoginErrorInvalidAuth -> do
        status status400
        json ("Invalid auth" :: Text)
      Left LoginErrorEmailNotVerified -> do
        status status400
        json ("Email not verified" :: Text)
      Right sId -> do
        setSessionIdInCookie sId
        return ()
  get "/api/users" $ do
    userId <- reqCurrentUserId
    mayEmail <- lift $ getUser userId
    case mayEmail of
      Nothing ->
        raise $ stringError "Should not happen: SessionId map to invalid UserId"
      Just email -> json $ rawEmail email
