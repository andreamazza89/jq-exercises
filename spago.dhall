{ name = "jq-exercises"
, dependencies =
  [ "aff"
  , "arrays"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "lists"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "spec"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
