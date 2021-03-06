module Adapter.HTTP.API.Common where

import           Adapter.HTTP.Common
import           ClassyPrelude
import           Data.Aeson                hiding (json)
import           Domain.Auth.Types
import           Network.HTTP.Types.Status
import qualified Text.Digestive.Aeson      as DF
import qualified Text.Digestive.Form       as DF
import qualified Text.Digestive.Types      as DF
import           Web.Scotty.Trans

parseAndValidateJSON ::
     (ScottyError e, MonadIO m, ToJSON v) => DF.Form v m a -> ActionT e m a
parseAndValidateJSON form = do
  val <- jsonData `rescue` (\_ -> return Null)
  validationResult <- lift $ DF.digestJSON form val
  case validationResult of
    (v, Nothing) -> do
      status status400
      json $ DF.jsonErrors v
      finish
    (_, Just result) -> return result

reqCurrentUserId :: (AuthService m, ScottyError e) => ActionT e m UserId
reqCurrentUserId = do
  mayUserId <- getCurrentUserId
  case mayUserId of
    Nothing -> do
      status status401
      json ("AuthRequired" :: Text)
      finish
    Just uId -> return uId
