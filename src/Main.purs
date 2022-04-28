module Main where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Error.Class (try)
import Control.Monad.Reader (runReaderT)
import Data.Array (last, null, tail)
import Data.Either (Either(..), either, hush)
import Data.Foldable (class Foldable, foldl, intercalate)
import Data.JSDate (getTime, now, toUTCString)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.MediaType (MediaType(..))
import Data.MediaType.Common as MIME
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Posix.Signal (Signal(..))
import Data.String (length)
import Data.String.Common (split, toLower)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.UUID (genUUID)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (MultipleErrors)
import HTTPure as HTTPure
import HTTPure.Request (Request)
import HTTPure.Response (ResponseM)
import Handler.Accounts (loadAccounts)
import Handler.Api.ApiHandler (Handler, HandlerEnv(..), handle)
import Handler.Api.CreateUser (CreateUser)
import Handler.Api.Logoff (Logoff)
import Handler.Api.Logon (Logon)
import Handler.Api.QueryUsers (QueryUsers)
import Manager.Account (shutdown)
import Manager.Account as Account
import Manager.Session as Session
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (onSignal)
import Record (delete)
import Type.Proxy (Proxy(..))

whitelistExtensions :: Array String
whitelistExtensions = [ "js", "html", "jpg", "png" ]

mimeTypes :: Map String MediaType
mimeTypes =
  Map.fromFoldable
    [ Tuple "js" MIME.applicationJavascript
    , Tuple "html" MIME.textHTML
    , Tuple "jpg" MIME.imageJPEG
    , Tuple "png" MIME.imagePNG
    , Tuple "map" MIME.textPlain
    ]

mimeType :: String -> Maybe MediaType
mimeType fileName = (last =<< (tail $ split (Pattern ".") fileName)) >>=
  flip Map.lookup mimeTypes <<< toLower

staticRoot :: String
staticRoot = "../client/dist"

loggingRouter :: HandlerEnv -> Request -> ResponseM
loggingRouter handlerEnv req = do
  startDate <- liftEffect $ now
  id <- liftEffect genUUID
  let
    idStr = " (" <> show id <> ")"
  let
    ts dt = "(" <> toUTCString dt <> ") "
  log $ ts startDate <> "Request received: " <> show req <> idStr
  response <- router handlerEnv req
  endDate <- liftEffect $ now
  let
    duration = getTime endDate - getTime startDate
  log $ ts endDate
    <> "Response is: "
    <> (show $ delete (Proxy :: _ "writeBody") response)
    <> " ["
    <> show duration
    <> " ms]"
    <> idStr
  pure $ response

oneOf :: ∀ a t f. Foldable f => Alt t => NonEmpty f (t a) -> t a
oneOf (NonEmpty x xs) = foldl (<|>) x xs

tryHandlers :: Request -> NonEmpty Array (Either MultipleErrors Handler)
tryHandlers { body } =
  handle (Proxy :: Proxy Logon)
    :| [ handle (Proxy :: Proxy Logoff)
      , handle (Proxy :: Proxy CreateUser)
      , handle (Proxy :: Proxy QueryUsers)
      ]
    <#> (\f -> f body)

staticFiles :: Request -> ResponseM
staticFiles { path } =
  let fileName =
        if null path then "index.html"
        else intercalate "/" path in mimeType fileName # maybe HTTPure.forbidden \mime -> do
          fileData' <- try $ readTextFile ASCII $ staticRoot <> "/" <> fileName
          fileData' # either (const HTTPure.notFound) \fileData ->
            let headers = HTTPure.headers
                  [ Tuple "Content-Length" (show $ length fileData)
                  , Tuple "Content-Type" $ unwrap mime
                  ] in
            HTTPure.ok' headers fileData

router :: HandlerEnv -> Request -> ResponseM
router handlerEnv req@{ method, body }
  | method == HTTPure.Post = case hush $ oneOf $ tryHandlers req of
    Nothing -> HTTPure.badRequest body
    Just x -> runReaderT x handlerEnv
  | method == HTTPure.Get = staticFiles req
  | otherwise = HTTPure.methodNotAllowed

port :: Int
port = 3000

main :: Effect Unit
main =
  launchAff_ do
    accountsAvar <- AVar.empty
    accountsArrayEither <- loadAccounts
    case accountsArrayEither of
      Left parserError -> log $ show parserError
      Right accountsArray -> do
        Account.startup accountsAvar accountsArray
        sessionsAvar <- Session.startup
        liftEffect do
          shutdownServer <-
            HTTPure.serve port (loggingRouter (HandlerEnv { accountsAvar, sessionsAvar }))
              $ log
              $ "Server up on http://localhost:"
              <> show port
          let
            terminate = do
              log "shutting down server"
              launchAff_ do
                shutdown accountsAvar
                Session.shutdown sessionsAvar
              shutdownServer $ log "server shut down"
          onSignal SIGINT terminate
          onSignal SIGTERM terminate
