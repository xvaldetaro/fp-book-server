module Handler.Api.ApiHandler where

import Prelude

import Control.Monad.Reader (ReaderT)
import Data.Either (Either)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Foreign (MultipleErrors)
import HTTPure (ResponseM, Response)
import Manager.Account (Accounts)
import Type.Proxy (Proxy)

newtype HandlerEnv = HandlerEnv {
  accountsAvar :: AVar Accounts
}

type Handler = ReaderT HandlerEnv Aff Response

class ApiHandler :: ∀ k. k -> Constraint
class ApiHandler a where
  handle :: String -> Proxy a -> Either MultipleErrors Handler