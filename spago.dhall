{ name = "jq-exercises"
, dependencies =
  [ "effect"
  , "exceptions"
  , "jq-interpreter"
  , "maybe"
  , "prelude"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
