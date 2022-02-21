module App.Pages.Home
  ( mkExercise
  , mkHome
  , sampleExercise
  ) where

import Prelude
import Data.Array (concat) as Array
import Data.String as String
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import JQ as JQ
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture, targetValue)
import React.Basic.Hooks (Component, Reducer, component, mkReducer, useReducer, (/\))
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

inputChanged dispatch buildAction = capture targetValue (fromMaybe "does this ever happen?" >>> buildAction >>> dispatch)

-- Exercise Component
sampleExercise :: Exercise
sampleExercise =
  { json: "{\"foo\": [42]}"
  , solution: [ "42.0" ]
  }

type Exercise
  = { json :: String
    , solution :: Array String
    }

type ExerciseState
  = { exerciseInput :: String
    }

data ExerciseAction
  = ExerciseInputUpdated String

data ViewExercise
  = NotStarted
  | FailedToRun String
  | Failed (Array String) (Array String)
  | Success (Array String)

initialExerciseState :: ExerciseState
initialExerciseState = { exerciseInput: "" }

exerciseReducerFn :: Effect (Reducer ExerciseState ExerciseAction)
exerciseReducerFn =
  mkReducer
    ( \s action -> case action of
        ExerciseInputUpdated newInput -> s { exerciseInput = newInput }
    )

mkExercise :: Component Exercise
mkExercise = do
  reducer <- exerciseReducerFn
  component "Exercise" \exercise -> React.do
    state /\ dispatch <- useReducer initialExerciseState reducer
    pure
      $ DOM.div
          { children:
              [ DOM.h1_ [ DOM.text "Exercise" ]
              , DOM.p_ [ (DOM.text "the exercise description goes here")] -- would be nice to have something like markdown. Maybe there's a webcomponent like that?") ]
              , DOM.textarea { value: exercise.json, disabled: true }
              , DOM.textarea
                  { value: state.exerciseInput
                  , onChange: inputChanged dispatch ExerciseInputUpdated
                  }
              , outcome exercise state
              ]
          }

outcome exercise state = case toViewExercise exercise state of
  NotStarted -> DOM.p_ [ DOM.text "" ] -- is there a null-equivalent for JSX?
  FailedToRun reason -> DOM.p_ [ DOM.text ("Could not run: " <> reason) ]
  Failed given expected -> DOM.p_ [ DOM.text ("Expected output: " <> String.joinWith " " expected <> ". Your solution gives: " <> String.joinWith " " given) ]
  Success output -> DOM.p_ [ DOM.text ("yayy") ]

toViewExercise exercise state =
  if (state.exerciseInput == "") then
    NotStarted
  else case JQ.run exercise.json state.exerciseInput of
    Just output -> checkSolution output
    Nothing -> FailedToRun "something did not parser or the interpreter failed"
  where
  checkSolution output =
    if output == exercise.solution then
      Success output
    else
      Failed output exercise.solution
