module Data.Api.Account where

import Prelude

import Api.User (UserRow)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype Account = Account { | UserRow (passwordHash :: String) }

derive instance genericAccount :: Generic Account _
instance showAccount :: Show Account where
  show = genericShow
