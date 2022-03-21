module Api.QueryUsers where

import Prelude
import Api.User (User(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

newtype QueryUsersRequest
  = QueryUsersRequest UUID

data QueryUsersResults
  = QueryUsersResultsFailure
    { reason :: QueryUsersResultsFailureReason
    }
  | QueryUsersResultsSuccess
    { users :: Array User
    }

data QueryUsersResultsFailureReason
  = NotAuthorized
  | NotAuthenticated

newtype QueryUsersResponse
  = QueryUsersResponse QueryUsersResults

-- Derive boilerplate --
derive instance genericQueryUsersRequest :: Generic QueryUsersRequest _

instance encodeQueryUsersRequest :: Encode QueryUsersRequest where
  encode = genericEncode defaultOptions

instance decodeQueryUsersRequest :: Decode QueryUsersRequest where
  decode = genericDecode defaultOptions

instance showQueryUsersRequest :: Show QueryUsersRequest where
  show = genericShow

derive instance genericQueryUsersResponse :: Generic QueryUsersResponse _

instance encodeQueryUsersResponse :: Encode QueryUsersResponse where
  encode = genericEncode defaultOptions

instance decodeQueryUsersResponse :: Decode QueryUsersResponse where
  decode = genericDecode defaultOptions

instance showQueryUsersResponse :: Show QueryUsersResponse where
  show = genericShow

derive instance genericQueryUsersResults :: Generic QueryUsersResults _

instance encodeQueryUsersResults :: Encode QueryUsersResults where
  encode = genericEncode defaultOptions

instance decodeQueryUsersResults :: Decode QueryUsersResults where
  decode = genericDecode defaultOptions

instance showQueryUsersResults :: Show QueryUsersResults where
  show = genericShow

derive instance genericQueryUsersResultsFailureReason :: Generic QueryUsersResultsFailureReason _
instance encodeQueryUsersResultsFailureReason :: Encode QueryUsersResultsFailureReason where
  encode = genericEncode defaultOptions
instance decodeQueryUsersResultsFailureReason :: Decode QueryUsersResultsFailureReason where
  decode = genericDecode defaultOptions
instance showQueryUsersResultsFailureReason :: Show QueryUsersResultsFailureReason where
  show = genericShow