module Api.CreateUserRequest where

import Prelude

import Api.User (User, UserRow)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

newtype CreateUserRequest
  = CreateUserRequest
  { authToken :: UUID
  , user :: { | UserRow (password :: String) }
  }

newtype CreateUserResponse
  = CreateUserResponse CreateUserResults

data CreateUserResults
  = CreateUserResultsSuccess
  | CreateUserResultsFailure
    { reason :: CreateUserResultsFailureReason
    }

data CreateUserResultsFailureReason
  = AlreadyExists
  | NotAuthenticated
  | NotAuthorized

derive instance genericCreateUserRequest :: Generic CreateUserRequest _

instance encodeCreateUserRequest :: Encode CreateUserRequest where
  encode = genericEncode defaultOptions

instance decodeCreateUserRequest :: Decode CreateUserRequest where
  decode = genericDecode defaultOptions

instance showCreateUserRequest :: Show CreateUserRequest where
  show = genericShow

derive instance genericCreateUserResponse :: Generic CreateUserResponse _

instance encodeCreateUserResponse :: Encode CreateUserResponse where
  encode = genericEncode defaultOptions

instance decodeCreateUserResponse :: Decode CreateUserResponse where
  decode = genericDecode defaultOptions

instance showCreateUserResponse :: Show CreateUserResponse where
  show = genericShow

derive instance genericCreateUserResults :: Generic CreateUserResults _

instance encodeCreateUserResults :: Encode CreateUserResults where
  encode = genericEncode defaultOptions

instance decodeCreateUserResults :: Decode CreateUserResults where
  decode = genericDecode defaultOptions

instance showCreateUserResults :: Show CreateUserResults where
  show = genericShow

derive instance genericCreateUserResultsFailureReason :: Generic CreateUserResultsFailureReason _

instance encodeCreateUserResultsFailureReason :: Encode CreateUserResultsFailureReason where
  encode = genericEncode defaultOptions

instance decodeCreateUserResultsFailureReason :: Decode CreateUserResultsFailureReason where
  decode = genericDecode defaultOptions

instance showCreateUserResultsFailureReason :: Show CreateUserResultsFailureReason where
  show = genericShow
