module App.Pages.AllExercises where

import Prelude
import App.DomUtils (container, h2)
import App.Exercises (Exercise)
import App.Exercises as Exercises
import Navigation (Navigation)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component)

mkAllExercises :: Component Navigation
mkAllExercises = do
  component "AllExercises" \nav -> React.do
    pure
      $ container
          ( [ h2 "Here's a list of all the exercises available:" ]
              <> links nav
          )

links :: Navigation -> Array JSX
links nav = (map (toLink nav) Exercises.all)

toLink :: Navigation -> Exercise -> JSX
toLink navigation exercise =
  DOM.article_
    [ DOM.a
        { children: [ DOM.text exercise.name ]
        , onClick: navigation.exercise exercise
        }
    ]
