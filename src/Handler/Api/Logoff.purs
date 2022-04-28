module Handler.Api.Logoff where

import Prelude

import Api.Logoff (LogoffRequest(..), LogoffResponse(..), LogoffResults(..))
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Effect.Class.Console (log)
import Foreign.Generic (encodeJSON)
import HTTPure as HTTPure
import Handler.Api.ApiHandler (class ApiHandler, Handler, HandlerEnv(..))
import Handler.Api.Common (handleApi)
import Manager.Session (deleteSession, verifySession)

data Logoff = Logoff

instance apiHandlerLogoff :: ApiHandler Logoff where
  handle _ = handleApi handler

handler :: LogoffRequest -> Handler
handler (LogoffRequest { authToken }) = do
  log $ "in logoff handler"
  HandlerEnv { sessionsAvar } <- ask
  verifiedSessionMaybe <- lift $ verifySession sessionsAvar authToken
  results <- case verifiedSessionMaybe of
    Nothing -> pure LogoffResultsFailure
    Just _ -> do
      lift $ deleteSession sessionsAvar authToken
      pure LogoffResultsSuccess
  HTTPure.ok $ encodeJSON $ LogoffResponse results

