module Main where

import Prelude

import Api.Logon (LogonRequest(..), LogonResponse(..), LogonResults(..))
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty ((:|))
import Data.Posix.Signal (Signal(..))
import Data.UUID (emptyUUID)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign.Generic (F, decodeJSON, encodeJSON)
import HTTPure (Method(..), Path, ServerM, (!!), (!?), (!@))
import HTTPure as HTTPure
import HTTPure.Request (Request)
import HTTPure.Response (ResponseM)
import Handler.Api.ApiHandler (class ApiHandler, handle)
import Handler.Api.Logon (Logon)
import Node.Process (onSignal)
import Test (test)
import Type.Proxy (Proxy(..))
import Undefined (undefined)

data OtherwisePath = OtherwisePath
instance apiHandlerOtherwisePath :: ApiHandler OtherwisePath where
  handle _ _ = Right $ HTTPure.ok "otherwise path"

router :: Request -> ResponseM
router req@{ path, method, body } = do
  -- stringBody <- toString body
  let
    eitherResponse = oneOf [
      hush $ handle body (Proxy :: Proxy Logon),
      hush $ handle "" (Proxy :: Proxy OtherwisePath)
    ]
  case eitherResponse of
    Nothing -> HTTPure.ok "Nothing"
    Just x -> x

port :: Int
port = 3000

main :: Effect Unit
main = do
  shutdown <-
    HTTPure.serve port router
      $ log
      $ "Server up on http://localhost:"
      <> show port
  let
    terminate = do
      log "shutting down server"
      shutdown $ log "server shut down"
  onSignal SIGINT terminate
  onSignal SIGTERM terminate
