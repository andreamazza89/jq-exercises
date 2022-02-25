module Navigation where

import App.Exercises (Exercise)
import React.Basic.Events (EventHandler)


type Navigation
  = { exercise :: Exercise -> EventHandler
    , allExercises :: EventHandler
    , home :: EventHandler
    }