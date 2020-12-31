module Lib where

import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.Auth as PG
import ClassyPrelude
import Control.Monad.Catch (MonadThrow)
import qualified Control.Monad.Fail as Fail
import Domain.Auth
import Katip

-- Throwaway code

-- instance AuthRepo IO where
--   addAuth (Auth email pass) = do
--     putStrLn $ "Adding auth: " <> rawEmail email
--     return $ Right "fake verification code"

instance EmailVerificationNotifier IO where
  notifyEmailVerification email vcode =
    putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode

type State = (PG.State, TVar M.State)

newtype App a = App
  { unApp :: ReaderT State (KatipContextT IO) a
  }
  deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, Fail.MonadFail, KatipContext, Katip, MonadThrow)

run :: LogEnv -> State -> App a -> IO a
run le state =
  runKatipContextT le () mempty
    . flip runReaderT state
    . unApp

instance AuthRepo App where
  addAuth = PG.addAuth
  setEmailAsVerified = PG.setEmailAsVerified
  findUserByAuth = PG.findUserByAuth
  findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotifier App where
  notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
  newSession = M.newSession
  findUserIdBySessionId = M.findUserIdBySessionId

action :: App ()
action = do
  let email = either undefined id $ mkEmail "hello@example.com"
      passwd = either undefined id $ mkPassword "helloWorld123123"
      auth = Auth email passwd
  register auth
  Just vCode <- M.getNotificationsForEmail email
  verifyEmail vCode
  Right session <- login auth
  Just uId <- resolveSessionId session
  Just registeredEmail <- getUser uId
  print (session, uId, registeredEmail)

someFunc :: IO ()
-- someFunc = do
--   state <- newTVarIO M.initialState
--   run state action
-- someFunc = withKatip $ \le -> do
--   state <- newTVarIO M.initialState
--   run le state action

someFunc = withKatip $ \le -> do
  mState <- newTVarIO M.initialState
  PG.withState pgCfg $ \pgState ->
    run le (pgState, mState) action
  where
    pgCfg =
      PG.Config
        { PG.configUrl = "postgresql://postgres:postgres@localhost/hauth",
          PG.configStripeCount = 2,
          PG.configMaxOpenConnPerStripe = 5,
          PG.configIdleConnTimeout = 10
        }

runKatip :: IO ()
runKatip = withKatip $ \le ->
  runKatipContextT le () mempty logSomething

withKatip :: (LogEnv -> IO a) -> IO a
withKatip =
  bracket createLogEnv closeScribes
  where
    createLogEnv = do
      logEnv <- initLogEnv "HAuth" "dev"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

logSomething :: (KatipContext m) => m ()
logSomething = do
  $(logTM) InfoS "Log in no space"
  katipAddNamespace "ns1" $
    $(logTM) InfoS "Under ns1"
  return ()
