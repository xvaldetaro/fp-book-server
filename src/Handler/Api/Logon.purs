module Handler.Api.Logon (Logon) where

import Prelude

import Api.Logon (LogonRequest(..))
import Control.Monad.Except (runExcept)
import Foreign.Generic (F, decodeJSON)
import HTTPure (Request, ResponseM)
import HTTPure as HTTPure
import Handler.Api.ApiHandler (class ApiHandler)

data Logon = Logon

instance apiHandlerLogon :: ApiHandler Logon where
  handle body _ = handler <$> runExcept (decodeJSON body :: F LogonRequest)

handler :: LogonRequest -> ResponseM
handler (LogonRequest req) = HTTPure.ok $ "logon request: " <> req.userName

