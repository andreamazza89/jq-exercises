{ name = "jq-exercises"
, dependencies =
  [ "arrays"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "jq-interpreter"
  , "js-timers"
  , "maybe"
  , "prelude"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "tuples"
  , "unsafe-coerce"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
