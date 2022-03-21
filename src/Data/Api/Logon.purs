module Api.Logon where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

newtype LogonRequest
  = LogonRequest
  { userName :: String
  , password :: String
  }

data LogonResults
  = LogonResultsSuccess
    { authToken :: UUID
    , mustChangePassword :: Boolean
    }
  | LogonResultsFailure

newtype LogonResponse
  = LogonResponse LogonResults

-- DERIVE BOILERPLATE --
derive instance genericLogonRequest :: Generic LogonRequest _

instance encodeLogonRequest :: Encode LogonRequest where
  encode = genericEncode defaultOptions

instance decodeLogonRequest :: Decode LogonRequest where
  decode = genericDecode defaultOptions

instance showLogonRequest :: Show LogonRequest where
  show = genericShow

derive instance genericLogonResults :: Generic LogonResults _

instance encodeLogonResults :: Encode LogonResults where
  encode = genericEncode defaultOptions

instance decodeLogonResults :: Decode LogonResults where
  decode = genericDecode defaultOptions

instance showLogonResults :: Show LogonResults where
  show = genericShow

derive instance genericLogonResponse :: Generic LogonResponse _

instance encodeLogonResponse :: Encode LogonResponse where
  encode = genericEncode defaultOptions

instance decodeLogonResponse :: Decode LogonResponse where
  decode = genericDecode defaultOptions

instance showLogonResponse :: Show LogonResponse where
  show = genericShow
