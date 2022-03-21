module Api.Logoff where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)
import Data.UUID (UUID)

newtype LogoffRequest = LogoffRequest UUID
derive instance genericLogoffRequest :: Generic LogoffRequest _
instance encodeLogoffRequest :: Encode LogoffRequest where
  encode = genericEncode defaultOptions
instance decodeLogoffRequest :: Decode LogoffRequest where
  decode = genericDecode defaultOptions
instance showLogoffRequest :: Show LogoffRequest where
  show = genericShow

newtype LogoffResponse = LogoffResponse LogoffResults
derive instance genericLogoffResponse :: Generic LogoffResponse _
instance encodeLogoffResponse :: Encode LogoffResponse where
  encode = genericEncode defaultOptions
instance decodeLogoffResponse :: Decode LogoffResponse where
  decode = genericDecode defaultOptions
instance showLogoffResponse :: Show LogoffResponse where
  show = genericShow

data LogoffResults = LogoffResultsFailure | LogoffResultsSuccess
derive instance genericLogoffResults :: Generic LogoffResults _
instance encodeLogoffResults :: Encode LogoffResults where
  encode = genericEncode defaultOptions
instance decodeLogoffResults :: Decode LogoffResults where
  decode = genericDecode defaultOptions
instance showLogoffResults :: Show LogoffResults where
  show = genericShow