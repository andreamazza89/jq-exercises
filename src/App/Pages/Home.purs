module App.Pages.Home
  ( mkApp
  , mkExercise
  , mkHome
  ) where

import App.Exercises (Exercise)
import App.Exercises as Exercises
import Prelude
import Data.Array (all, length, zip) as Array
import Data.Either (Either(..), either)
import Data.Maybe (fromMaybe, maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import JQ as JQ
import React.Basic.DOM (css)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture_, capture, targetValue)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Component, JSX, Reducer, component, mkReducer, useReducer, useState, (/\))
import React.Basic.Hooks as React

-- 
data AppState
  = Home
  | Exercise Exercise
  | AllExercises

mkApp :: Component Unit
mkApp = do
  home <- mkHome
  exercise <- mkExercise
  allExercises <- mkAllExercises
  component "App" \_ -> React.do
    state /\ updateState <- useState Home
    let
      navigation =
        { exercise: \ex -> capture_ $ updateState (const $ Exercise ex)
        , allExercises: capture_ $ updateState (const AllExercises)
        , home: capture_ $ updateState (const Home)
        }
    pure
      $ case state of
          Home -> home navigation
          Exercise ex -> exercise ex
          AllExercises -> allExercises navigation

type Navigation
  = { exercise :: Exercise -> EventHandler
    , allExercises :: EventHandler
    , home :: EventHandler
    }

--
mkAllExercises :: Component Navigation
mkAllExercises = do
  component "AllExercises" \nav -> React.do
    pure
      $ DOM.div
          { className: "container"
          , children:
              [ DOM.h1_ [ DOM.text "Here's a list of all the exercises available:" ] ]
                <> ( map
                      ( \ex ->
                          DOM.a
                            { children: [ DOM.text ex.name ]
                            , onClick: nav.exercise ex
                            }
                      )
                      Exercises.all
                  )
          }

--
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
              [ DOM.h1_ [ DOM.text "Jq - exercises" ]
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

inputChanged dispatch buildAction = capture targetValue (fromMaybe "does this ever happen?" >>> buildAction >>> dispatch)

-- Exercise Component
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
          { className: "container"
          , children:
              [ DOM.h1_ [ DOM.text "Exercise" ]
              , DOM.p_ [ (DOM.text "the exercise description goes here") ]
              , DOM.textarea { value: exercise.json, disabled: true }
              , DOM.textarea
                  { value: state.exerciseInput
                  , onChange: inputChanged dispatch ExerciseInputUpdated
                  , placeholder: "Your JQ code goes here"
                  }
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
      , DOM.div
          { children:
              [ showJson "Output from your Expression" given
              , showJson "Expected Output" expected
              ]
          , className: "grid"
          }
      ]
  Success output ->
    DOM.div_
      [ successMessage "Success!"
      , showJson "Output from your Expression" output
      ]

errorMessage :: String -> JSX
errorMessage = textWithColor "#fd5050"

successMessage :: String -> JSX
successMessage = textWithColor "#21c782"

textWithColor :: String -> String -> JSX
textWithColor color text = DOM.h4 { children: [ DOM.text text ], style: (css { color }) }

showJson :: String -> Array String -> JSX
showJson label json =
  DOM.article_
    [ DOM.header_ [ DOM.text (label <> ":") ]
    , DOM.ul_ $ map (\j -> DOM.li_ [ DOM.text j ]) json
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
              if Array.length exercise.solution == Array.length output && Array.all identity comparisons then
                Success output
              else
                Failed output exercise.solution
          )

  compareJsonStrings (Tuple l r) = JQ.jsonEquals l r
