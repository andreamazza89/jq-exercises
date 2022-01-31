{ name = "jq-exercises"
, dependencies =
  [ "aff"
  , "arrays"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
