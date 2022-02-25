module App.Pages.Home
  ( mkHome
  ) where

import App.DomUtils (errorMessage, h1, inputChanged, showJson)
import Prelude
import App.Exercises as Exercises
import Data.Either (either)
import Effect (Effect)
import JQ as JQ
import Navigation (Navigation)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, Reducer, component, mkReducer, useReducer, (/\))
import React.Basic.Hooks as React

type HomeState
  = { jsonInput :: String
    , expressionInput :: String
    }

data Action
  = JsonInputUpdated String
  | ExpressionInputUpdated String

initialState :: HomeState
initialState =
  { jsonInput: "{\"foo\": 42}"
  , expressionInput: ".foo"
  }

reducerFn :: Effect (Reducer HomeState Action)
reducerFn =
  mkReducer
    ( \s action -> case action of
        JsonInputUpdated newInput -> s { jsonInput = newInput }
        ExpressionInputUpdated newInput -> s { expressionInput = newInput }
    )

mkHome :: Component Navigation
mkHome = do
  reducer <- reducerFn
  component "Home" \nav -> React.do
    state /\ dispatch <- useReducer initialState reducer
    pure
      $ DOM.div
          { className: "container"
          , children:
              [ h1 "Jq - exercises"
              , DOM.a
                  { children: [ DOM.text "Go to the first exercise" ]
                  , onClick: nav.exercise (Exercises.first)
                  }
              , DOM.a
                  { children: [ DOM.text "View all exercises available" ]
                  , onClick: nav.allExercises
                  }
              , DOM.textarea
                  { value: state.jsonInput
                  , onChange: inputChanged dispatch JsonInputUpdated
                  }
              , DOM.textarea
                  { value: state.expressionInput
                  , onChange: inputChanged dispatch ExpressionInputUpdated
                  }
              , DOM.div_
                  ( either (\reason -> [ errorMessage $ "Something went wrong: " <> reason ])
                      (\output -> [ showJson "Output from your Expression" output ])
                      (JQ.run state.jsonInput state.expressionInput)
                  )
              ]
          }
