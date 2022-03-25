module Main where

import Prelude

import Api.Logon (LogonRequest(..), LogonResponse(..), LogonResults(..))
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..), hush)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty ((:|))
import Data.Posix.Signal (Signal(..))
import Data.UUID (emptyUUID)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign.Generic (F, decodeJSON, encodeJSON)
import HTTPure (Method(..), Path, ServerM, (!!), (!?), (!@))
import HTTPure as HTTPure
import HTTPure.Request (Request)
import HTTPure.Response (ResponseM)
import Handler.Accounts (bootstrapAccount, loadAccounts)
import Handler.Api.ApiHandler (class ApiHandler, HandlerEnv(..), handle)
import Handler.Api.Logon (Logon)
import Manager.Account (Accounts, makeHash, startup)
import Node.Process (onSignal)
import Test (test)
import Text.Parsing.Parser (runParser)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

-- data OtherwisePath = OtherwisePath
-- instance apiHandlerOtherwisePath :: ApiHandler OtherwisePath where
--   handle _ _ = Right $ HTTPure.ok "otherwise path"

router :: AVar Accounts -> Request -> ResponseM
router accountsAvar req@{ path, method, body } =
  flip runReaderT (HandlerEnv {accountsAvar}) do
    -- let
    --   eitherResponse = oneOf [
    let eitherResponse = handle body (Proxy :: Proxy Logon)
        -- handle "" (Proxy :: Proxy OtherwisePath)
      -- ]
    case eitherResponse of
      Left y -> HTTPure.ok $ "Error " <> (show y)
      Right x -> x

port :: Int
port = 3000

main :: Effect Unit
main = launchAff_ do
  accountsAvar <- AVar.empty
  accountsArrayEither <- loadAccounts
  case accountsArrayEither of
    Left parserError -> log $ show parserError
    Right accountsArray -> do
      startup accountsAvar accountsArray
      void $ liftEffect $ HTTPure.serve port (router accountsAvar) (pure unit)
      pure unit
        -- $ log
        -- $ "Server up on http://localhost:"
        -- <> show port
  -- let
  --   terminate = do
  --     log "shutting down server"
  --     shutdown $ log "server shut down"
  -- launchAff_ do
  --   accounts <- loadAccounts
  --   log $ show accounts
  -- onSignal SIGINT terminate
  -- onSignal SIGTERM terminate

