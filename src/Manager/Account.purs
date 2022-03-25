module Manager.Account where

import Prelude
import Data.Api.Account (Account(..))
import Data.Array (foldr)
import Data.Char (toCharCode)
import Data.Map (Map, fromFoldable, lookup, member, values)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar, put, take)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Crypto.Hash (Algorithm(..), createHash, digest, update)
import Node.Crypto.Hash as Algorithm
import Node.Encoding (Encoding(..))
import Random.LCG (Seed, mkSeed)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (sample)
import Undefined (undefined)

type Accounts
  = Map String Account

startup :: AVar Accounts -> Array Account -> Aff Unit
startup avar accs =
  accs
    <#> (\acc@(Account { userName }) -> Tuple userName acc)
    # fromFoldable
    # flip AVar.put avar

shutdown :: AVar Accounts -> Aff Unit
shutdown avar = void $ take avar

verifyLogon :: AVar Accounts -> String -> String -> Aff (Maybe Account)
verifyLogon avar userName password = do
  account <- take avar <#> lookup userName
  passwordHash' <- makeHash password userName
  pure $ account
    >>= ( \acc@(Account { passwordHash }) ->
          if (passwordHash' == passwordHash) then Just acc else Nothing
      )

makeHash :: String -> String -> Aff String
makeHash str saltOrigin =
  liftEffect do
    let
      salted = str <> salt 5 saltOrigin
    buf <- Buffer.fromString salted ASCII
    emptyHash <- createHash SHA256
    hash <- update emptyHash buf
    hashBuf <- digest hash
    Buffer.toString Hex hashBuf

salt :: Int -> String -> String
salt length str = sample (seedFromString str) length arbitrary # fromCharArray

seedFromString :: String -> Seed
seedFromString str = str # toCharArray <#> toCharCode # sumArray # mkSeed
  where
  sumArray = foldr (*) 0
