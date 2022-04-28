module Manager.Account where

import Prelude

import Api.User (User(..))
import Crypto (passwordHashHex)
import Data.Api.Account (Account(..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map, fromFoldable, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar, take)
import Effect.Aff.AVar as AVar
import Record as Record
import Type.Proxy (Proxy(..))
import Utils as Utils

type Accounts
  = Map String Account

startup :: AVar Accounts -> Array Account -> Aff Unit
startup avar accs =
  accs
    <#> (\acc@(Account { userName }) -> Tuple userName acc)
    # fromFoldable
    # flip AVar.put avar

shutdown :: AVar Accounts -> Aff Unit
shutdown avar = void $ take avar

verifyLogon :: AVar Accounts -> String -> String -> Aff (Maybe Account)
verifyLogon avar userName password = do
  accounts <- take avar
  AVar.put accounts avar
  let account = lookup userName accounts
  passwordHash' <- passwordHashHex password userName
  pure $ account
    >>= ( \acc@(Account { passwordHash }) ->
          if (passwordHash' == passwordHash) then Just acc else Nothing
      )


getAccounts :: AVar Accounts -> Aff (Array Account)
getAccounts avar = AVar.read avar <#> (Map.values >>> Array.fromFoldable)

findAccount :: AVar Accounts -> String -> Aff (Maybe Account)
findAccount avar userName = do
  accounts <- AVar.read avar
  pure $ Map.lookup userName accounts

data CreateAccountError = CreateAccountAlreadyExists

createAccount :: AVar Accounts -> Account -> Aff (Either CreateAccountError Unit)
createAccount avar acc@(Account {userName}) =
  Utils.withAVar avar
    \accounts -> pure $
      if Map.member userName accounts then
        Tuple accounts (Left CreateAccountAlreadyExists)
      else
        Tuple (Map.insert userName acc accounts) (Right unit)
