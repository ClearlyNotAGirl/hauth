module Domain.Validation where

import           ClassyPrelude
import           Text.Regex.PCRE.Heavy

type Validation e a = a -> Maybe e

validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate builder validations value =
  case concatMap (\f -> maybeToList $ f value) validations of
    []   -> Right $ builder value
    errs -> Left errs

rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
rangeBetween min max msg val =
  if val >= min && val <= max
    then Nothing
    else Just msg

lengthBetween :: (MonoFoldable a) => Int -> Int -> e -> Validation e a
lengthBetween minLen maxLen msg val =
  rangeBetween minLen maxLen msg (length val)

regexMatches :: Regex -> e -> Validation e Text
regexMatches regex msg val =
  if val =~ regex
    then Nothing
    else Just msg
