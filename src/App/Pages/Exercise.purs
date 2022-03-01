module App.Pages.Exercise (mkExercise) where

import Prelude
import App.DomUtils (button, container, errorMessage, h2, inputChanged, row, showJson, showJsons, successMessage)
import App.Exercises (Exercise)
import App.Exercises (next) as Exercises
import Data.Array (all, length, zip) as Array
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import JQ as JQ
import Navigation (Navigation)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Component, JSX, Reducer, component, mkReducer, useReducer, (/\))
import React.Basic.Hooks as React
import WebComponents.Markdown as Markdown

type ExerciseProps
  = { exercise :: Exercise
    , navigation :: Navigation
    }

type ExerciseState
  = { exerciseInput :: String
    }

data ExerciseAction
  = ExerciseInputUpdated String
  | ResetInput

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
        ResetInput -> s { exerciseInput = "" }
    )

reset :: (ExerciseAction -> Effect Unit) -> EventHandler
reset dispatch = capture_ (dispatch ResetInput)

mkExercise :: Component ExerciseProps
mkExercise = do
  reducer <- exerciseReducerFn
  component "Exercise" \{ exercise, navigation } -> React.do
    state /\ dispatch <- useReducer initialExerciseState reducer
    let
      nextExercise =
        Exercises.next exercise
          # maybe
              (mempty)
              (\ex -> button "Go to the next exercise" (reset dispatch <> navigation.exercise ex))

      viewAll = button "View all exercises" navigation.allExercises
    pure
      $ container
          [ row [ nextExercise, viewAll ]
          , h2 exercise.name
          , Markdown.build exercise.description
          , row
              [ showJson exercise.json
              , DOM.textarea
                  { value: state.exerciseInput
                  , onChange: inputChanged dispatch ExerciseInputUpdated
                  , placeholder: "Your JQ code goes here"
                  }
              ]
          , outcome exercise state
          ]

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
          [ showJsons "Output from your Expression" given
          , showJsons "Expected Output" expected
          ]
      ]
  Success output ->
    DOM.div_
      [ successMessage "Success!"
      , showJsons "Output from your Expression" output
      ]

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
    Array.zip output exercise.solution
      # traverse compareJsonStrings
      # maybe
          (Failed output exercise.solution)
          ( \comparisons ->
              if sameLength exercise.solution output && Array.all identity comparisons then
                Success output
              else
                Failed output exercise.solution
          )

  compareJsonStrings (Tuple l r) = JQ.jsonEquals l r

  sameLength l r = Array.length l == Array.length r