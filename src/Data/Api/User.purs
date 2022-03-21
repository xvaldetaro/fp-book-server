module Api.User where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

type UserRow r
  = ( userName :: String
    , temporaryPassword :: Boolean
    , admin :: Boolean
    , firstName :: String
    , lastName :: String
    | r
    )

newtype User
  = User { | UserRow () }

-- derive boilerplate --
derive instance genericUser :: Generic User _

instance encodeUser :: Encode User where
  encode = genericEncode defaultOptions

instance decodeUser :: Decode User where
  decode = genericDecode defaultOptions

instance showUser :: Show User where
  show = genericShow
