module Test.Main where

import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude (Unit, ($), discard)
import Test.Interpreter (main) as Interpreter
import Test.Parser (main) as Parser
import Test.PrattParser as PrattParser
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        Parser.main
        Interpreter.main
        PrattParser.main
