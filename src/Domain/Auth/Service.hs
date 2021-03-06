module Domain.Auth.Service where

import           ClassyPrelude
import           Control.Monad.Except
import           Domain.Auth.Types
import           Katip

class Monad m =>
      AuthRepo m
  where
  addAuth :: Auth -> m (Either RegistrationError (UserId, VerificationCode))
  setEmailAsVerified ::
       VerificationCode -> m (Either EmailVerificationError (UserId, Email))
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m =>
      EmailVerificationNotifier m
  where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

class Monad m =>
      SessionRepo m
  where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

withUserIdContext :: (KatipContext m) => UserId -> m a -> m a
withUserIdContext uId = katipAddContext (sl "userId" uId)

register ::
     (KatipContext m, AuthRepo m, EmailVerificationNotifier m)
  => Auth
  -> m (Either RegistrationError ())
register auth =
  runExceptT $ do
    (uId, vCode) <- ExceptT $ addAuth auth
    let email = authEmail auth
    lift $ notifyEmailVerification email vCode
    withUserIdContext uId $
      $(logTM) InfoS $ ls (rawEmail email) <> " is registered successfully"

verifyEmail ::
     (KatipContext m, AuthRepo m)
  => VerificationCode
  -> m (Either EmailVerificationError ())
verifyEmail vCode =
  runExceptT $ do
    (uId, email) <- ExceptT $ setEmailAsVerified vCode
    withUserIdContext uId $
      $(logTM) InfoS $ ls (rawEmail email) <> " is verified successfully"
    return ()

login ::
     (KatipContext m, AuthRepo m, SessionRepo m)
  => Auth
  -> m (Either LoginError SessionId)
login auth =
  runExceptT $ do
    result <- lift $ findUserByAuth auth
    case result of
      Nothing -> throwError LoginErrorInvalidAuth
      Just (_, False) -> throwError LoginErrorEmailNotVerified
    -- Just (uid, _) -> lift $ newSession uid
      Just (uId, _) ->
        withUserIdContext uId . lift $ do
          sId <- newSession uId
          $(logTM) InfoS $
            ls (rawEmail . authEmail $ auth) <> " logged in successfully"
          return sId

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId
