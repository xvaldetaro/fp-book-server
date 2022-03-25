module Handler.Accounts where

import Prelude

import Data.Api.Account (Account(..))
import Data.Array (intercalate)
import Data.Bifunctor (lmap)
import Data.Either (Either, fromRight)
import Data.String (joinWith)
import Data.String.Utils (lines)
import Data.Traversable (sequence)
import Effect.Aff (Aff, try)
import Foreign.NullOrUndefined (undefined)
import Manager.Account (makeHash)
import Node.Encoding as Encoding
import Node.FS.Aff (appendTextFile, exists, readTextFile, writeTextFile)
import Parser.Account (parseAccount)
import Text.Parsing.Parser (ParseError)

filename :: String
filename = "accounts.csv"

bootstrapAccount :: Aff String
bootstrapAccount = do
  let userName = "admin"
      pw = "admin"
      true' = show true
  passwordHash <- makeHash pw userName
  pure $ intercalate "," [userName, passwordHash, true', true', "Joe", "Admin" ]

loadAccounts :: Aff (Either ParseError (Array Account))
loadAccounts = parseCsv <$> loadOrCreateFile

parseCsv :: String -> Either ParseError (Array Account)
parseCsv s = sequence $ parseAccount <$> lines s

loadOrCreateFile :: Aff String
loadOrCreateFile = do
  doesExist <- try $ exists filename
  unless (fromRight false doesExist) do
    acc <- bootstrapAccount
    writeTextFile Encoding.UTF8 filename acc
  readTextFile Encoding.UTF8 filename

createAccount :: Account -> Aff (Either String Unit)
createAccount acc =
  let result = try $ appendTextFile Encoding.UTF8 filename (accountToCsv acc) in
  lmap show <$> result

accountToCsv :: Account -> String
accountToCsv (Account
  { userName
  , passwordHash
  , temporaryPassword
  , admin
  , firstName
  , lastName
  }) = joinWith "," [
    userName,
    passwordHash,
    show temporaryPassword,
    show admin,
    firstName,
    lastName
  ]
