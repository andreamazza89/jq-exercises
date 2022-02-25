module App (mkApp) where

import App.Pages.AllExercises (mkAllExercises)
import App.Pages.Exercise (mkExercise)
import App.Pages.Home (mkHome)
import Prelude

import App.Exercises (Exercise)
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React

{-
  This is the main component for the frontend, which figures out what page should display
  and provides navigation callbacks to the pages.

  It is definitely naive and not very scalable, but we want to keep this very simple as the
  app is unlikely to become much more complex than this.
-}

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
