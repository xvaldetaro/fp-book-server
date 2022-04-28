module Handler.Api.ApiHandler where


import Control.Monad.Reader (ReaderT)
import Data.Either (Either)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Foreign (MultipleErrors)
import HTTPure (Response)
import Manager.Account (Accounts)
import Manager.Session (Sessions)
import Type.Proxy (Proxy)

newtype HandlerEnv = HandlerEnv
  { accountsAvar :: AVar Accounts
  , sessionsAvar :: AVar Sessions
  }

type Handler = ReaderT HandlerEnv Aff Response

class ApiHandler :: ∀ k. k -> Constraint
class ApiHandler a where
  handle :: Proxy a -> String -> Either MultipleErrors Handler