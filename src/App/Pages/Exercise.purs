module App.Pages.Exercise (mkExercise) where

import Prelude
import App.DomUtils (errorMessage, h2, inputChanged, row, showJson, successMessage)
import App.Exercises (Exercise)
import Data.Array (all, length, zip) as Array
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Timer (clearInterval, setInterval)
import JQ as JQ
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, Reducer, component, mkReducer, useReducer, useEffect, (/\))
import React.Basic.Hooks as React
import WebComponents.Markdown as Markdown

type ExerciseState
  = { exerciseInput :: String
    , latest :: String
    }

data ExerciseAction
  = ExerciseInputUpdated String
  | Synch

data ViewExercise
  = NotStarted
  | FailedToRun String
  | Failed (Array String) (Array String)
  | Success (Array String)

initialExerciseState :: ExerciseState
initialExerciseState = { exerciseInput: "", latest: "" }

exerciseReducerFn :: Effect (Reducer ExerciseState ExerciseAction)
exerciseReducerFn =
  mkReducer
    ( \s action -> case action of
        ExerciseInputUpdated newInput -> s { exerciseInput = newInput }
        Synch -> s { latest = s.exerciseInput }
    )

mkExercise :: Component Exercise
mkExercise = do
  reducer <- exerciseReducerFn
  component "Exercise" \exercise -> React.do
    state /\ dispatch <- useReducer initialExerciseState reducer
    useEffect state do
      timeoutId <- setInterval 500 (dispatch Synch)
      pure (clearInterval timeoutId)
    pure
      $ DOM.div
          { className: "container"
          , children:
              [ h2 exercise.name
              , Markdown.build exercise.description
              , row
                  [ DOM.textarea { value: exercise.json, disabled: true }
                  , DOM.textarea
                      { value: state.exerciseInput
                      , onChange: inputChanged dispatch ExerciseInputUpdated
                      , placeholder: "Your JQ code goes here"
                      }
                  ]
              , outcome exercise state
              ]
          }

outcome :: Exercise -> ExerciseState -> JSX
outcome exercise state = case toViewExercise exercise state of
  NotStarted -> mempty
  FailedToRun reason ->
    DOM.div_
      [ errorMessage "Could not parse or run expression"
      , DOM.p_ [ DOM.text reason ]
      ]
  Failed given expected ->
    DOM.div_
      [ errorMessage "Not quite, try again"
      , row
          [ showJson "Output from your Expression" given
          , showJson "Expected Output" expected
          ]
      ]
  Success output ->
    DOM.div_
      [ successMessage "Success!"
      , showJson "Output from your Expression" output
      ]

toViewExercise :: Exercise -> ExerciseState -> ViewExercise
toViewExercise exercise state =
  if (state.latest == "") then
    NotStarted
  else case JQ.run exercise.json state.latest of
    Right output -> checkSolution output
    Left reason -> FailedToRun $ "Something went wrong: " <> reason
  where
  checkSolution output =
    -- because JQ's output is an array of json strings, here we
    --   first zip together the given/expected outputs,
    --   compare each pair's JSON
    --   succeed if all pairs match
    Array.zip output exercise.solution
      # traverse compareJsonStrings
      # maybe
          (Failed output exercise.solution)
          ( \comparisons ->
              if Array.length exercise.solution == Array.length output && Array.all identity comparisons then
                Success output
              else
                Failed output exercise.solution
          )

  compareJsonStrings (Tuple l r) = JQ.jsonEquals l r
