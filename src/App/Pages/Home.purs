module App.Pages.Home
  ( mkHome
  ) where

import Prelude
import App.DomUtils (container, button, errorMessage, inputChanged, row, showJsons)
import App.DebouncedInput
import App.Exercises as Exercises
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import JQ as JQ
import Navigation (Navigation)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, Reducer, component, mkReducer, useReducer, (/\))
import React.Basic.Hooks as React
import WebComponents.Markdown as Markdown

type HomeState
  = { jsonInput :: String
    , expressionInput :: DebouncedInput
    }

data Action
  = JsonInputUpdated String
  | ExpressionInputUpdated String
  | Bounce DebouncedInput

initialState :: HomeState
initialState =
  { jsonInput: """{"iLove": "bread"}"""
  , expressionInput: init ".iLove"
  }

reducerFn :: Effect (Reducer HomeState Action)
reducerFn =
  mkReducer
    ( \s action -> case action of
        JsonInputUpdated newInput -> s { jsonInput = newInput }
        ExpressionInputUpdated newInput -> s { expressionInput = addValue newInput s.expressionInput }
        Bounce previous -> s { expressionInput = debounce previous s.expressionInput }
    )

mkHome :: Component Navigation
mkHome = do
  reducer <- reducerFn
  component "Home" \nav -> React.do
    state /\ dispatch <- useReducer initialState reducer
    bounce state.expressionInput (Bounce >>> dispatch)
    pure
      $ container
          [ row
              [ button "Go to the first exercise" (nav.exercise (Exercises.first))
              , button "View all exercises" nav.allExercises
              ]
          , Markdown.build appDescription
          , row
              [ DOM.textarea
                  { value: state.jsonInput
                  , onChange: inputChanged dispatch JsonInputUpdated
                  }
              , DOM.textarea
                  { value: fromMaybe "" $ latestValue state.expressionInput
                  , onChange: inputChanged dispatch ExpressionInputUpdated
                  }
              ]
          , DOM.div_
              ( either
                  (\reason -> [ errorMessage $ "Something went wrong: " <> reason ])
                  (\output -> [ showJsons "Output from your Expression" output ])
                  (JQ.run state.jsonInput (fromMaybe "" $ debouncedValue state.expressionInput))
              )
          ]

appDescription :: String
appDescription =
  """
  Welcome to _Learn JQ_, where you can explore the jq language via a series of exercises.

  JQ is a marvellous language specialised to manipulate JSON data.
  From the [language website](https://stedolan.github.io/jq/):

  > jq is like sed for JSON data - you can use it to slice and filter and map and
  > transform structured data with the same ease that sed, awk, grep and friends let you play with text.

  #### Notice
  We only support a __small subset__ of the jq language, so if something is missing, that's because
  it's not implemented yet. You can ask for it to be added by
  opening an issue in [this project's repo](https://github.com/andreamazza89/jq-exercises/issues/new).

  Also, the error messages are sometimes misleading / not helpful. Improving this area is on our roadmap,
  but until then - apologies.

  ---

  Below is a repl to just play with the language: you can change the input json, the jq expression and
  see the result below.
"""
