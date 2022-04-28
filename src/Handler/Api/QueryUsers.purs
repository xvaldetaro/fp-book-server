module Handler.Api.QueryUsers where

import Prelude

import Api.QueryUsers (QueryUsersRequest(..), QueryUsersResponse(..), QueryUsersResults(..))
import Api.QueryUsers as QueryUsers
import Api.User (User(..))
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Api.Account (Account(..))
import Data.Api.Session (Session(..))
import Data.Either (either, note)
import Effect.Class.Console (log)
import Foreign.Generic (encodeJSON)
import HTTPure as HTTPure
import Handler.Api.ApiHandler (class ApiHandler, Handler, HandlerEnv(..))
import Handler.Api.Common (handleApi)
import Manager.Account (findAccount, getAccounts)
import Manager.Session (verifySession)
import Record as Record
import Type.Proxy (Proxy(..))
import Utils (liftSuccess)

data QueryUsers = QueryUsers

instance apiHandlerQueryUsers :: ApiHandler QueryUsers where
  handle _ = handleApi handler

handler :: QueryUsersRequest -> Handler
handler (QueryUsersRequest { authToken }) = do
  log $ "in QueryUsers handler"
  HandlerEnv { sessionsAvar, accountsAvar } <- ask
  result <- lift $ runExceptT do
    Session { userName } <- verifySession sessionsAvar authToken
      <#> note QueryUsers.NotAuthenticated # liftSuccess
    Account {admin} <- findAccount accountsAvar userName
      <#> note QueryUsers.NotAuthorized # liftSuccess
    unless admin $ throwError QueryUsers.NotAuthorized
    lift $ getAccounts accountsAvar <#> map toUser
  HTTPure.ok $ encodeJSON $ QueryUsersResponse $ result #
    either
      (\reason -> QueryUsersResultsFailure { reason })
      (\users -> QueryUsersResultsSuccess { users })

toUser :: Account -> User
toUser (Account acc) = User $ Record.delete (Proxy :: _ "passwordHash") acc