module Handler.Api.Logon (Logon) where

import Prelude

import Api.Logon (LogonRequest(..), LogonResponse(..), LogonResults(..))
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Api.Account (Account(..))
import Data.Maybe (Maybe(..))
import Data.UUID (emptyUUID)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Foreign.Generic (F, decodeJSON, encodeJSON)
import HTTPure (Request, ResponseM)
import HTTPure as HTTPure
import Handler.Api.ApiHandler (class ApiHandler, Handler, HandlerEnv(..))
import Manager.Account (Accounts, verifyLogon)
import Undefined (undefined)

data Logon = Logon

instance apiHandlerLogon :: ApiHandler Logon where
  handle body _ = handler <$> runExcept (decodeJSON body :: F LogonRequest)

handler :: LogonRequest -> Handler
handler (LogonRequest {userName, password}) = do
  HandlerEnv { accountsAvar } <- ask
  lift do
    accountMaybe <- verifyLogon accountsAvar userName password
    case accountMaybe of
      Nothing -> HTTPure.ok $ encodeJSON $ LogonResponse LogonResultsFailure
      Just (Account acc) -> HTTPure.ok $ encodeJSON
        $ LogonResponse
        $ LogonResultsSuccess {authToken: emptyUUID, mustChangePassword: acc.temporaryPassword}



