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

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either EmailValidationErr ())

verifyEmail :: AuthRepo m => VerificationCode -> m (Either EmailValidationErr ())
verifyEmail = setEmailAsVerified

class Monad m => EmailVerificationNotifier m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

register :: (AuthRepo m, EmailVerificationNotifier m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  vCode <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode
