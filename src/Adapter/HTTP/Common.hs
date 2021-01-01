module Adapter.HTTP.Common where

import Blaze.ByteString.Builder (toLazyByteString)
import ClassyPrelude
import Data.Aeson hiding (json)
import Data.Time.Lens
import Domain.Auth.Types
import Network.HTTP.Types.Status
import qualified Text.Digestive.Aeson as DF
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DF
import Web.Cookie
import Web.Scotty.Trans
import Katip

type WebContext m = (MonadIO m, KatipContext m, AuthService m)

toResult :: Either e a -> DF.Result e a
toResult = either DF.Error DF.Success

setCookie :: (ScottyError e, Monad m) => SetCookie -> ActionT e m ()
setCookie =
  setHeader "Set-Cookie" . decodeUtf8 . toLazyByteString . renderSetCookie

getCookie :: (ScottyError e, Monad m) => Text -> ActionT e m (Maybe Text)
getCookie key = do
  mCookieStr <- header "Cookie"
  return $ do
    cookie <- parseCookies . encodeUtf8 . toStrict <$> mCookieStr
    let bsKey = encodeUtf8 key
    val <- lookup bsKey cookie
    return $ decodeUtf8 val

setSessionIdInCookie :: (MonadIO m, ScottyError e) => SessionId -> ActionT e m ()
setSessionIdInCookie sId = do
  curTime <- liftIO getCurrentTime
  setCookie $
    def
      { setCookieName = "sId",
        setCookiePath = Just "/",
        setCookieValue = encodeUtf8 sId,
        setCookieExpires = Just $ modL month (+ 1) curTime,
        setCookieHttpOnly = True,
        setCookieSecure = False,
        setCookieSameSite = Just sameSiteLax
      }

getCurrentUserId :: (AuthService m, ScottyError e) => ActionT e m (Maybe UserId)
getCurrentUserId = do
  maySessionId <- getCookie "sId"
  case maySessionId of
    Nothing -> return Nothing
    Just sId -> lift $ resolveSessionId sId

errorResponse :: (ToJSON a) => a -> Value
errorResponse val = object ["error" .= val]