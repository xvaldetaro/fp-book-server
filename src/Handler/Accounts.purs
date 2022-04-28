module Handler.Accounts where

import Prelude

import Crypto (passwordHashHex)
import Data.Api.Account (Account(..))
import Data.Array (intercalate)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), fromRight)
import Data.String (joinWith)
import Data.String.Utils (lines)
import Data.Traversable (sequence)
import Effect.Aff (Aff, try)
import Foreign.NullOrUndefined (undefined)
import Node.Encoding as Encoding
import Node.FS.Aff (appendTextFile, exists, readTextFile, writeTextFile)
import Parser.Account (parseAccounts)
import Text.Parsing.Parser (ParseError)

filename :: String
filename = "accounts.csv"

bootstrapAccount :: Aff String
bootstrapAccount = do
  let
    userName = "admin"
    pw = "admin"
  passwordHash <- passwordHashHex pw userName
  pure $ accountToCsv
    $ Account
        { userName
        , passwordHash
        , admin: true
        , temporaryPassword: true
        , firstName: "Joe"
        , lastName: "Admin"
        }

loadAccounts :: Aff (Either ParseError (Array Account))
loadAccounts = loadOrCreateFile <#> parseAccounts

loadOrCreateFile :: Aff String
loadOrCreateFile = do
  doesExist <- try $ exists filename
  unless (fromRight false doesExist) do
    acc <- bootstrapAccount
    writeTextFile Encoding.UTF8 filename acc
  readTextFile Encoding.UTF8 filename

data CreateAccountError
  = CreateAccountFileError String

createAccount :: Account -> Aff (Either CreateAccountError Unit)
createAccount acc =
  do
    try $ appendTextFile Encoding.UTF8 filename (accountToCsv acc)
    <#> lmap (CreateAccountFileError <<< show)

accountToCsv :: Account -> String
accountToCsv ( Account
    { userName
  , passwordHash
  , temporaryPassword
  , admin
  , firstName
  , lastName
  }
) =
  joinWith ","
    [ userName
    , passwordHash
    , show temporaryPassword
    , show admin
    , firstName
    , lastName
    ]
    <> "\n"
