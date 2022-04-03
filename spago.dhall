{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "console"
  , "control"
  , "crypto"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "foreign-generic"
  , "httpure"
  , "js-date"
  , "lcg"
  , "maybe"
  , "node-buffer"
  , "node-fs-aff"
  , "node-process"
  , "nonempty"
  , "ordered-collections"
  , "parsing"
  , "posix-types"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "undefined"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
