{ name = "jq-exercises"
, dependencies =
  [ "aff"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "lists"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "spec"
  , "strings"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
