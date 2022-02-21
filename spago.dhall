{ name = "jq-exercises"
, dependencies =
  [ "arrays"
  , "effect"
  , "exceptions"
  , "jq-interpreter"
  , "maybe"
  , "prelude"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "strings"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
