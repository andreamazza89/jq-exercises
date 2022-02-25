{ name = "jq-exercises"
, dependencies =
  [ "arrays"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "jq-interpreter"
  , "maybe"
  , "prelude"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "tuples"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
