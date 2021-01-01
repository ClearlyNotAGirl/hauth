module Domain.Auth.Types (
  Auth(..),
  Email(),
  rawEmail,
  mkEmail,
  Password(),
  rawPassword,
  mkPassword,
  UserId,
  VerificationCode,
  SessionId,
  RegistrationError(..),
  EmailVerificationError(..),
  LoginError(..),

  AuthService(..)
) where

import ClassyPrelude
import Control.Monad.Except
import Domain.Validation
import Text.Regex.PCRE.Heavy

data RegistrationError
  = RegistrationErrorEmailTaken
  deriving (Show, Eq)

-- Email-related types

newtype Email = Email {emailRaw :: Text} deriving (Show, Eq, Ord)

rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email
mkEmail =
  validate
    Email
    [ regexMatches
        [re|^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,64}$|]
        "Not a valid email"
    ]

data EmailValidationErr = EmailValidationErrInvalidEmail deriving (Show, Eq)

-- Password-related types

newtype Password = Password {passwordRaw :: Text} deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword =
  validate
    Password
    [ lengthBetween 5 50 "Should between 5 and 50",
      regexMatches [re|\d|] "Should contain number",
      regexMatches [re|[A-Z]|] "Should contain uppercase letter",
      regexMatches [re|[a-z]|] "Should contain lowercase letter"
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

data EmailVerificationError = EmailVerificationErrorInvalidCode deriving (Show, Eq)

-- User related types

type UserId = Int -- deriving (Show, Eq)

type SessionId = Text

data LoginError
  = LoginErrorInvalidAuth
  | LoginErrorEmailNotVerified
  deriving (Show, Eq)

-- Implementations

class Monad m => AuthService m where
  register :: Auth -> m (Either RegistrationError ())
  verifyEmail :: VerificationCode -> m (Either EmailVerificationError ())
  login :: Auth -> m (Either LoginError SessionId)
  resolveSessionId :: SessionId -> m (Maybe UserId)
  getUser :: UserId -> m (Maybe Email)
