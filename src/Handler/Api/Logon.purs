module Handler.Api.Logon (Logon) where

import Prelude

import Api.Logon (LogonRequest(..), LogonResponse(..), LogonResults(..))
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Api.Account (Account(..))
import Data.Maybe (Maybe(..))
import Effect.Class.Console (log)
import Foreign.Generic (encodeJSON)
import HTTPure as HTTPure
import Handler.Api.ApiHandler (class ApiHandler, HandlerEnv(..), Handler)
import Handler.Api.Common (handleApi)
import Manager.Account (verifyLogon)
import Manager.Session (createSession)

data Logon
  = Logon

instance apiHandlerLogon :: ApiHandler Logon where
  handle _ = handleApi handler

handler :: LogonRequest -> Handler
handler (LogonRequest { userName, password }) = do
  HandlerEnv { sessionsAvar, accountsAvar } <- ask
  log $ "in logon handler"
  accountMaybe <- lift $ verifyLogon accountsAvar userName password
  log $ "vefiryLogon called"
  results <- case accountMaybe of
    Nothing -> pure LogonResultsFailure
    Just (Account acc) -> do
      log $ "will get sessionUuid"
      sessionUuid <- lift $ createSession sessionsAvar acc.userName
      log $ "got sessionUuid"
      pure $ LogonResultsSuccess { authToken: sessionUuid, mustChangePassword: acc.temporaryPassword }
  HTTPure.ok $ encodeJSON $ LogonResponse results
