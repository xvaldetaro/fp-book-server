module Crypto where

import Prelude

import Data.Array (foldr)
import Data.Char (toCharCode)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer as Buffer
import Node.Crypto.Hash (Algorithm(..), createHash, digest, update)
import Node.Encoding (Encoding(..))
import Random.LCG (Seed, mkSeed)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (sample)

passwordHashHex :: String -> String -> Aff String
passwordHashHex str saltOrigin =
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