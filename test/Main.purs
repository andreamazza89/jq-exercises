module Test.Main where

import Prelude (Unit, ($))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Parser (main) as Parser
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        Parser.main
