module Adapter.InMemory.Auth where

import           ClassyPrelude
import           Control.Monad.Except
import           Data.Has
import qualified Domain.Auth.Types    as D
import           Text.StringRandom

data State =
  State
    { stateAuths            :: [(D.UserId, D.Auth)]
    , stateUnverifiedEmails :: Map D.VerificationCode D.Email
    , stateVerifiedEmails   :: Set D.Email
    , stateUserIdCounter    :: Int
    , stateNotifications    :: Map D.Email D.VerificationCode
    , stateSessions         :: Map D.SessionId D.UserId
    }
  deriving (Show, Eq)

initialState :: State
initialState =
  State
    { stateAuths = []
    , stateUnverifiedEmails = mempty
    , stateVerifiedEmails = mempty
    , stateUserIdCounter = 0
    , stateNotifications = mempty
    , stateSessions = mempty
    }

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

addAuth ::
     InMemory r m
  => D.Auth
  -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth auth = do
  tvar <- asks getter
  vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  atomically . runExceptT $ do
    state <- lift $ readTVar tvar
    let auths = stateAuths state
        email = D.authEmail auth
        isDuplicate = any (email ==) . map (D.authEmail . snd) $ auths
    when isDuplicate $ throwError D.RegistrationErrorEmailTaken
    -- actual update
    let newUserId = stateUserIdCounter state + 1
        newAuths = (newUserId, auth) : auths
        unverifieds = stateUnverifiedEmails state
        newUnverified = insertMap vCode email unverifieds
        newState =
          state
            { stateAuths = newAuths
            , stateUserIdCounter = newUserId
            , stateUnverifiedEmails = newUnverified
            }
    lift $ writeTVar tvar newState
    -- return vCode
    return (newUserId, vCode)

orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow Nothing e  = throwError e
orThrow (Just a) _ = return a

setEmailAsVerified ::
     InMemory r m
  => D.VerificationCode
  -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
  tvar <- asks getter
  atomically . runExceptT $ do
    state :: State <- lift $ readTVar tvar
    let unverifiedEmails = stateUnverifiedEmails state
        maybeEmail = lookup vCode unverifiedEmails
    email :: D.Email <- maybeEmail `orThrow` D.EmailVerificationErrorInvalidCode
    let auths = stateAuths state
        maybeUserId = map fst . find ((email ==) . D.authEmail . snd) $ auths
    uId <- maybeUserId `orThrow` D.EmailVerificationErrorInvalidCode
    let verified = stateVerifiedEmails state
        newVerified = insertSet email verified
        newUnverified = deleteMap vCode unverifiedEmails
        newState =
          state
            { stateUnverifiedEmails = newUnverified
            , stateVerifiedEmails = newVerified
            }
    lift $ writeTVar tvar newState
    return (uId, email)

-- setEmailAsVerified :: TVar State -> D.VerificationCode -> IO (Either D.EmailVerificationError ())
-- setEmailAsVerified vCode = do
findUserByAuth :: InMemory r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth auth = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let maybeUserId = map fst . find ((auth ==) . snd) $ stateAuths state
  case maybeUserId of
    Nothing -> return Nothing
    Just uId -> do
      let verifiedEmails = stateVerifiedEmails state
          email = D.authEmail auth
          isVerified = elem email verifiedEmails
      return $ Just (uId, isVerified)

findEmailFromUserId :: InMemory r m => D.UserId -> m (Maybe D.Email)
findEmailFromUserId uId = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  let maybeAuth = map snd . find ((uId ==) . fst) $ stateAuths state
  return $ D.authEmail <$> maybeAuth

notifyEmailVerification :: InMemory r m => D.Email -> D.VerificationCode -> m ()
notifyEmailVerification email vCode = do
  tvar <- asks getter
  atomically $ do
    state <- readTVar tvar
    let notifications = stateNotifications state
        newNotifications = insertMap email vCode notifications
        newState = state {stateNotifications = newNotifications}
    writeTVar tvar newState

newSession :: InMemory r m => D.UserId -> m D.SessionId
newSession uId = do
  tvar <- asks getter
  sId <- liftIO $ (tshow uId <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
  atomically $ do
    state <- readTVar tvar
    let sessions = stateSessions state
        newSessions = insertMap sId uId sessions
        newState = state {stateSessions = newSessions} -- TODO: Use lens
    writeTVar tvar newState
    return sId

findUserIdBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
  tvar <- asks getter
  liftIO $ lookup sId . stateSessions <$> readTVarIO tvar

getNotificationsForEmail ::
     InMemory r m => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  return $ lookup email $ stateNotifications state
