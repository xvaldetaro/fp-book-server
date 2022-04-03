module Main where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Array (head)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (oneOf, (:|))
import Data.Posix.Signal (Signal(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPure as HTTPure
import HTTPure.Request (Request)
import HTTPure.Response (ResponseM)
import Handler.Accounts (loadAccounts)
import Handler.Api.ApiHandler (HandlerEnv(..), handle)
import Handler.Api.Logon (Logon)
import Manager.Account (Accounts, shutdown, startup)
import Node.Process (onSignal)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

router :: AVar Accounts -> Request -> ResponseM
router accountsAvar req@{ path, method, body }
  | method == HTTPure.Post =
    -- head will give us a Maybe (Either ). Binding hush with flatten the Maybes
    case hush =<< (head $ oneOf $ (handle body (Proxy :: Proxy Logon)) :| []) of
      Nothing -> HTTPure.badRequest body
      Just x -> runReaderT x (HandlerEnv {accountsAvar})
  | otherwise = HTTPure.methodNotAllowed

port :: Int
port = 3002

main :: Effect Unit
main = launchAff_ do
  accountsAvar <- AVar.empty
  accountsArrayEither <- loadAccounts
  case accountsArrayEither of
    Left parserError -> log $ show parserError
    Right accountsArray -> do
      startup accountsAvar accountsArray
      liftEffect do
        shutdownServer <- HTTPure.serve port (router accountsAvar)
          $ log $ "Server up on http://localhost:" <> show port
        let
          terminate = do
            log "shutting down server"
            launchAff_ $ shutdown accountsAvar
            shutdownServer $ log "server shut down"
        onSignal SIGINT terminate
        onSignal SIGTERM terminate

