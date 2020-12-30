module Domain.Auth where

import ClassyPrelude
import Control.Monad.Except
import Domain.Validation
import Text.Regex.PCRE.Heavy

data RegistrationError
  = RegistrationErrorEmailTaken
  deriving (Show, Eq)

-- Email-related types

newtype Email = Email {emailRaw :: Text} deriving (Show, Eq)

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [EmailValidationErr] Email
mkEmail =
  validate
    Email
    [ regexMatches
        [re|^([a-zA-Z0-9_\-\.]+)@([a-zA-Z0-9_\-\.]+)\.([a-zA-Z]{2,5})$|]
        EmailValidationErrInvalidEmail
    ]

data EmailValidationErr = EmailValidationErrInvalidEmail deriving (Show, Eq)

-- Password-related types

newtype Password = Password {passwordRaw :: Text} deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [PasswordValidationErr] Password
mkPassword =
  validate
    Password
    [ lengthBetween 5 50 PasswordValidationErrLength,
      regexMatches [re|\d|] PasswordValidationErrMustContainNumber,
      regexMatches [re|[A-Z]|] PasswordValidationErrMustContainUpperCase,
      regexMatches [re|[a-z]|] PasswordValidationErrMustContainLowerCase
    ]

data PasswordValidationErr
  = PasswordValidationErrLength
  | PasswordValidationErrMustContainUpperCase
  | PasswordValidationErrMustContainLowerCase
  | PasswordValidationErrMustContainNumber
  deriving (Show, Eq)

-- Auth-related types

data Auth = Auth
  { authEmail :: Email,
    authPassword :: Password
  }
  deriving (Show, Eq)

-- Registration

type VerificationCode = Text

data EmailVerificatioNRError = EmailVerificationErrorInvalidCode deriving (Show, Eq)

class Monad m => EmailVerificationNotifier m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

register :: (AuthRepo m, EmailVerificationNotifier m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  vCode <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode

-- User related types

newtype UserId = UserId Int

type SessionId = Text

data LoginError
  = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Show, Eq)

-- Implementations

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either EmailValidationErr ())
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)

getUser :: AuthRepo m => UserId -> m (Maybe Email)
getUser = findEmailFromUserId

verifyEmail :: AuthRepo m => VerificationCode -> m (Either EmailValidationErr ())
verifyEmail = setEmailAsVerified

login :: (AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (uid, _) -> lift $ newSession uid

class Monad m => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

-- Misc
