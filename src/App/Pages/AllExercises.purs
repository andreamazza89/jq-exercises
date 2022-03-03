module App.Pages.AllExercises where

import Prelude
import App.DomUtils (button, container, h2, row)
import App.Exercises (Exercise)
import App.Exercises as Exercises
import Data.Array as Array
import Navigation (Navigation)
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
links navigation =
  chunk 3 Exercises.all
    # map (toRow navigation)

toRow :: Navigation -> Array Exercise -> JSX
toRow navigation exercises =
  row (map (toLink navigation) exercises)

toLink :: Navigation -> Exercise -> JSX
toLink navigation exercise =
  button exercise.name (navigation.exercise exercise)

-- Array utils (this is the only one in the frontend, but if we create more we should move them into
-- a Utils module)
chunk :: forall a . Int -> Array a -> Array (Array a)
chunk size arr =
  chunk_ size arr []

chunk_ :: forall a . Int -> Array a -> Array (Array a) -> Array (Array a)
chunk_ _ [] output =
  output
chunk_ size arr output =
  chunk_ size (Array.drop size arr) (output <> [Array.take size arr])
