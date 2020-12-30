module Adapter.InMemory.Auth where

import ClassyPrelude
import Data.Has
import qualified Domain.Auth as D
import Text.StringRandom

data State = State
  { stateAuths :: [(D.UserId, D.Auth)],
    stateUnverifiedEmails :: Map D.VerificationCode D.Email,
    stateVerifiedEmails :: Set D.Email,
    stateUserIdCounter :: Int,
    stateNotifications :: Map D.Email D.VerificationCode,
    stateSessions :: Map D.SessionId D.UserId
  }
  deriving (Show, Eq)

initialState :: State
initialState =
  State
    { stateAuths = [],
      stateUnverifiedEmails = mempty,
      stateVerifiedEmails = mempty,
      stateUserIdCounter = 0,
      stateNotifications = mempty,
      stateSessions = mempty
    }

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

-- I'm gonna Google this one real quick, to see what where to put it.
-- Yeah it stays here.
addAuth :: InMemory r m => D.Auth -> m (Either D.RegistrationError D.VerificationCode)
addAuth = undefined

-- setEmailAsVerified :: InMemory r m => D.VerificationCode -> m (Either D.EmailValidationErr ())
-- setEmailAsVerified = undefined

setEmailAsVerified :: TVar State -> D.VerificationCode -> IO (Either D.EmailVerificationError ())
setEmailAsVerified = undefined

findUserByAuth :: InMemory r m => D.Auth -> m (Maybe (D.UserId, Bool))
findUserByAuth = undefined

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

getNotificationsForEmail :: InMemory r m => D.Email -> m (Maybe D.VerificationCode)
getNotificationsForEmail email = do
  tvar <- asks getter
  state <- liftIO $ readTVarIO tvar
  return $ lookup email $stateNotifications state