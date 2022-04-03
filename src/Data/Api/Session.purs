module Data.Api.Session where

import Prelude

import Api.User (UserRow)
import Data.Api.Account (Account(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID)

newtype Session
  = Session
  { authToken :: UUID
  , userName :: String
  , lastTime :: Number
  }