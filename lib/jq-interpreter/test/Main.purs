module Test.Main where

import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude (Unit, ($), discard)
import Test.Interpreter (main) as Interpreter
import Test.Json (main) as Json
import Test.Parser.Expression (main) as ExpressionParser
import Test.Parser.Environment (main) as EnvironmentParser
import Test.PrattParser as PrattParser
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        ExpressionParser.main
        Interpreter.main
        EnvironmentParser.main
        PrattParser.main
        Json.main
