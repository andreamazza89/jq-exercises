module App.Pages.Exercise (mkExercise) where

import App.DomUtils
import App.DebouncedInput
import Prelude
import App.Exercises (Exercise)
import App.Exercises (next) as Exercises
import Data.Array (all, length, zip) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
  = { exerciseInput :: DebouncedInput
    }

data ExerciseAction
  = ExerciseInputUpdated String
  | Bounce DebouncedInput
  | ResetInput

data ViewExercise
  = NotStarted
  | FailedToRun String
  | Failed (Array String) (Array String)
  | Success (Array String)

initialExerciseState :: ExerciseState
initialExerciseState = { exerciseInput: empty }

exerciseReducerFn :: Effect (Reducer ExerciseState ExerciseAction)
exerciseReducerFn =
  mkReducer
    ( \s action -> case action of
        ExerciseInputUpdated newInput -> s { exerciseInput = addValue newInput s.exerciseInput }
        ResetInput -> s { exerciseInput = empty }
        Bounce previous -> s { exerciseInput = debounce previous s.exerciseInput }
    )

reset :: (ExerciseAction -> Effect Unit) -> EventHandler
reset dispatch = capture_ (dispatch ResetInput)

mkExercise :: Component ExerciseProps
mkExercise = do
  reducer <- exerciseReducerFn
  component "Exercise" \{ exercise, navigation } -> React.do
    state /\ dispatch <- useReducer initialExerciseState reducer
    bounce state.exerciseInput (Bounce >>> dispatch)
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
                  { value: fromMaybe "" $ latestValue state.exerciseInput
                  , onChange: inputChanged dispatch ExerciseInputUpdated
                  , id: "jq-input"
                  , onFocus: capture_ (scrollJqInputIntoView)
                  , placeholder: "Your JQ code goes here"
                  }
              ]
          , outcome exercise state
          ]

outcome :: Exercise -> ExerciseState -> JSX
outcome exercise state = case toViewExercise exercise state of
  NotStarted -> extraHeight mempty
  FailedToRun reason ->
    extraHeight
      ( DOM.div_
          [ errorMessage "Could not parse or run expression"
          , DOM.p_ [ DOM.text reason ]
          ]
      )
  Failed given expected ->
    extraHeight
      ( DOM.div_
          [ errorMessage "Not quite, try again"
          , row
              [ showJsons "Output from your Expression" given
              , showJsons "Expected Output" expected
              ]
          ]
      )
  Success output ->
    extraHeight
      ( DOM.div_
          [ successMessage "Success!"
          , showJsons "Output from your Expression" output
          ]
      )

-- This is hacky and reveals a lack of good UI design: because the height of the content underneath the JQ expression input 
-- varies depending on the outcome, we set a fixed height here to prevent jumping up and down as the user types
extraHeight :: JSX -> JSX
extraHeight element = withMinHeight 1500 element

toViewExercise :: Exercise -> ExerciseState -> ViewExercise
toViewExercise exercise state = case debouncedValue state.exerciseInput of
  Nothing -> NotStarted
  Just input -> case JQ.run exercise.json input of
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
