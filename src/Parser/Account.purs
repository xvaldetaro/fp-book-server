module Parser.Account where

import Prelude

import Control.Alt ((<|>))
import Data.Api.Account (Account(..))
import Data.Array (catMaybes, fromFoldable)
import Data.Array.NonEmpty (some, toArray)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, singleton)
import Text.Parsing.Parser (ParseError, Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (sepBy)
import Text.Parsing.Parser.String (char, string)
import Text.Parsing.Parser.Token (alphaNum)

parseAccounts :: String -> Either ParseError (Array Account)
parseAccounts s = runParser s accountsParser

accountsParser :: Parser String (Array Account)
accountsParser = sepBy go (char '\n') <#> fromFoldable >>> catMaybes
  where
    go = (accParser <#> Just) <|> pure Nothing

accParser :: Parser String Account
accParser = do
  userName' <- userName
  passwordHash <- eatComma word
  temporaryPassword <- eatComma bool
  admin <- eatComma bool
  firstName <- eatComma word
  lastName <- eatComma word
  pure $ Account
    { userName: userName'
    , passwordHash
    , temporaryPassword
    , admin
    , firstName
    , lastName
    }

bool :: Parser String Boolean
bool = do
  result <- string "true" <|> string "false"
  case result of
    "true" -> pure true
    "false" -> pure false
    _ -> fail "bool parse fail"

eatComma :: ∀ a. Parser String a -> Parser String a
eatComma p = char ',' *> p

userName :: Parser String String
userName = append <<< singleton <$> alphaNum <*> word

word :: Parser String String
word = fromCharArray <<< toArray <$> some alphaNum

toBoolean :: String -> Boolean
toBoolean "true" = true
toBoolean _ = false