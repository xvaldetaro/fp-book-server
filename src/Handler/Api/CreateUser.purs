module Handler.Api.CreateUser where

import Prelude

import Api.CreateUserRequest (CreateUserRequest(..), CreateUserResponse(..), CreateUserResults(..), CreateUserResultsFailureReason(..))
import Api.User (UserRow)
import Control.Monad.Except (runExceptT, throwError, withExceptT)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Crypto (passwordHashHex)
import Data.Api.Account (Account(..))
import Data.Api.Session (Session(..))
import Data.Either (either, note)
import Effect.Aff (Aff)
import Foreign.Generic (encodeJSON)
import HTTPure as HTTPure
import Handler.Accounts (CreateAccountError(..))
import Handler.Accounts as AH
import Handler.Api.ApiHandler (class ApiHandler, Handler, HandlerEnv(..))
import Handler.Api.Common (handleApi)
import Manager.Account as AM
import Manager.Session (verifySession)
import Record as Record
import Type.Proxy (Proxy(..))
import Utils (liftSuccess)

data CreateUser = CreateUser

instance apiHandlerCreateUser :: ApiHandler CreateUser where
  handle _ = handleApi handler

handler :: CreateUserRequest -> Handler
handler (CreateUserRequest { user, authToken }) = do
  HandlerEnv { sessionsAvar, accountsAvar } <- ask
  result <- lift $ runExceptT do
    Session {userName} <- verifySession sessionsAvar authToken
      <#> note NotAuthenticated # liftSuccess
    Account {admin} <- AM.findAccount accountsAvar userName
      <#> note NotAuthorized # liftSuccess
    unless admin $ throwError NotAuthorized
    newAccount <- lift $ userToAccount user
    AM.createAccount accountsAvar newAccount
      # liftSuccess
      # withExceptT (const AlreadyExists)
    AH.createAccount newAccount
      # liftSuccess
      # withExceptT (\(CreateAccountFileError s) -> FileIOError s)
  HTTPure.ok $ encodeJSON $ CreateUserResponse $ result #
    either
      (\r -> CreateUserResultsFailure { reason : r })
      (const CreateUserResultsSuccess)

userToAccount :: { | UserRow (password :: String) } -> Aff Account
userToAccount row@{password, userName} = do
  hash <- passwordHashHex password userName
  let user = Record.delete (Proxy :: _ "password") row
  pure $ Account $ Record.insert (Proxy ::  _ "passwordHash") hash user
