module App.Pages.Home (mkHome) where

import Prelude
import Data.Maybe (fromMaybe, maybe)
import Effect (Effect)
import JQ as JQ
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture, targetValue)
import React.Basic.Hooks (Component, Reducer, component, mkReducer, useReducer, useState, (/\))
import React.Basic.Hooks as React

type HomeProps
  = Unit

type State
  = { jsonInput :: String
    , expressionInput :: String
    }

data Action
  = JsonInputUpdated String
  | ExpressionInputUpdated String

initialState :: State
initialState =
  { jsonInput: "{\"foo\": 42}"
  , expressionInput: ".foo"
  }

reducerFn :: Effect (Reducer State Action)
reducerFn =
  mkReducer
    ( \s action -> case action of
        JsonInputUpdated newInput -> s { jsonInput = newInput }
        ExpressionInputUpdated newInput -> s { expressionInput = newInput }
    )

mkHome :: Component HomeProps
mkHome = do
  reducer <- reducerFn
  component "Home" \_ -> React.do
    state' /\ dispatch <- useReducer initialState reducer
    state /\ setState <- useState ({ input: "{\"foo\": 42}", exp: ".foo" })
    pure
      $ DOM.div
          { children:
              [ DOM.h1_ [ DOM.text "Jq - exercises" ]
              , DOM.textarea
                  { value: state'.jsonInput
                  , onChange: inputChanged dispatch JsonInputUpdated
                  }
              , DOM.textarea
                  { value: state'.expressionInput
                  , onChange: inputChanged dispatch ExpressionInputUpdated
                  }
              , DOM.p_
                  ( maybe ([ DOM.text "no result yet" ])
                      (map DOM.text)
                      (JQ.run state'.jsonInput state'.expressionInput)
                  )
              ]
          }

inputChanged dispatch buildAction =
  capture targetValue (fromMaybe "does this ever happen?" >>> buildAction >>> dispatch)
