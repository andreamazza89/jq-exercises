module App.Pages.Home
  ( mkExercise
  , mkHome
  , sampleExercise
  )
  where

import Prelude

import Data.Array (concat, zip, all, sort) as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import JQ as JQ
import React.Basic.DOM (css)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture, targetValue)
import React.Basic.Hooks (Component, Reducer, component, mkReducer, useReducer, (/\))
import React.Basic.Hooks as React

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

mkHome :: Component Unit
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
                  ( either (\reason -> [ DOM.text $ "woops: " <> reason ])
                      (map DOM.text)
                      (JQ.run state'.jsonInput state'.expressionInput)
                  )
              ]
          }

inputChanged dispatch buildAction = capture targetValue (fromMaybe "does this ever happen?" >>> buildAction >>> dispatch)

-- Exercise Component
sampleExercise :: Exercise
sampleExercise =
  { json: "{\"foo\": [42, 43]}"
  , solution: [ "42.0", "43.0" ]
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
      $ DOM.section
          { children:
              [ DOM.h1_ [ DOM.text "Exercise" ]
              , DOM.p_ [ (DOM.text "the exercise description goes here") ]
              , DOM.textarea { value: exercise.json, disabled: true }
              , DOM.textarea
                  { value: state.exerciseInput
                  , onChange: inputChanged dispatch ExerciseInputUpdated
                  }
              , outcome exercise state
              ]
          }

outcome exercise state = case toViewExercise exercise state of
  NotStarted -> mempty
  FailedToRun reason -> DOM.p {children: [ DOM.text ("Could not run: " <> reason) ], style: (css { color: "red"}) }
  Failed given expected ->
    DOM.div
      { children:
          [ givenJson given
          , givenJson expected
          ]
      , className: "grid"
      }
  Success output -> DOM.div_ $ map (\json -> DOM.div { children: [ DOM.p_ [ DOM.text json ] ] }) output

givenJson given =
  DOM.div {
    children: [
      DOM.p_ [ DOM.text "Your JSON: "]
      , DOM.ul_  $ map (\j -> DOM.li_ [DOM.text j]) given
       
    ] 
  }
  

toViewExercise :: Exercise -> ExerciseState -> ViewExercise
toViewExercise exercise state =
  if (state.exerciseInput == "") then
    NotStarted
  else case JQ.run exercise.json state.exerciseInput of
    Right output -> checkSolution output
    Left reason -> FailedToRun $ "Something went wrong: " <> reason
  where
  checkSolution output =
    -- because JQ's output is an array of json strings, here we
    --   first zip together the given/expected outputs,
    --   compare each pair's JSON
    --   succeed if all pairs match
    Array.zip (Array.sort output) (Array.sort exercise.solution)
      # traverse compareJsonStrings
      # maybe
          (Failed output exercise.solution)
          ( \comparisons ->
              if Array.all (identity) comparisons then
                Success output
              else
                Failed output exercise.solution
          )

  compareJsonStrings (Tuple l r) = JQ.jsonEquals l r
