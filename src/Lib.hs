module Lib where

import Domain.Auth.Types
import qualified Domain.Auth.Service as D
import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.Auth as PG
import qualified Adapter.RabbitMQ.Auth as MQAuth
import qualified Adapter.RabbitMQ.Common as MQ
import qualified Adapter.Redis.Auth as Redis
import qualified Adapter.HTTP.Main as HTTP
import ClassyPrelude
import Control.Exception.Safe (MonadCatch)
import Control.Monad.Catch (MonadThrow)
import qualified Control.Monad.Fail as Fail
--import Domain.Auth
import Katip

-- Throwaway code

-- instance AuthRepo IO where
--   addAuth (Auth email pass) = do
--     putStrLn $ "Adding auth: " <> rawEmail email
--     return $ Right "fake verification code"

-- instance EmailVerificationNotifier IO where
--   notifyEmailVerification email vcode =
--     putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode

type State = (PG.State, Redis.State, MQ.State, TVar M.State)

newtype App a = App
  { unApp :: ReaderT State (KatipContextT IO) a
  }
  deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, Fail.MonadFail, KatipContext, Katip, MonadThrow, MonadCatch)

run :: LogEnv -> State -> App a -> IO a
run le state =
  runKatipContextT le () mempty
    . flip runReaderT state
    . unApp

instance AuthService App where
  register = D.register
  verifyEmail = undefined -- D.verifyEmail
  login = undefined -- D.login
  resolveSessionId = undefined -- D.resolveSessionId
  getUser = undefined -- D.getUser

instance MQAuth.EmailVerificationSender App where
  sendEmailVerification = M.notifyEmailVerification

-- instance AuthRepo App where
--   addAuth = PG.addAuth
--   setEmailAsVerified = PG.setEmailAsVerified
--   findUserByAuth = PG.findUserByAuth
--   findEmailFromUserId = PG.findEmailFromUserId

-- instance EmailVerificationNotifier App where
--   notifyEmailVerification = MQAuth.notifyEmailVerification

-- instance SessionRepo App where
--   newSession = Redis.newSession
--   findUserIdBySessionId = Redis.findUserIdBySessionId

action :: App ()
action = do
  let email = either undefined id $ mkEmail "hello@example.com"
      passwd = either undefined id $ mkPassword "helloWorld123123"
      auth = Auth email passwd
  register auth
  vCode <- pollNotif email
  verifyEmail vCode
  Right session <- login auth
  Just uId <- resolveSessionId session
  Just registeredEmail <- getUser uId
  print (session, uId, registeredEmail)
  where
    pollNotif email = do
      result <- M.getNotificationsForEmail email
      case result of
        Nothing -> pollNotif email
        Just vCode -> return vCode

-- someFunc :: IO ()
-- someFunc = do
--   state <- newTVarIO M.initialState
--   run state action
-- someFunc = withKatip $ \le -> do
--   state <- newTVarIO M.initialState
--   run le state action

withState :: (Int -> LogEnv -> State -> IO ()) -> IO ()
withState action =
  withKatip $ \le -> do
    mState <- newTVarIO M.initialState
    PG.withState pgCfg $ \pgState ->
      Redis.withState redisCfg $ \redisState ->
        MQ.withState mqCfg 16 $ \mqState -> do
          let state = (pgState, redisState, mqState, mState)
          action port le state
  where
    mqCfg = "amqp://guest:guest@localhost:5672/%2F"
    redisCfg = "redis://localhost:6379/0"
    pgCfg =
      PG.Config
        { PG.configUrl = "postgresql://postgres:postgres@localhost/hauth",
          PG.configStripeCount = 2,
          PG.configMaxOpenConnPerStripe = 5,
          PG.configIdleConnTimeout = 10
        }
    port = 3000

-- runKatip :: IO ()
-- runKatip = withKatip $ \le ->
--   runKatipContextT le () mempty logSomething

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
