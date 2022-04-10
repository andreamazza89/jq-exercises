module App.DebouncedInput
  ( DebouncedInput
  , addValue
  , bounce
  , debounce
  , debouncedValue
  , empty
  , init
  , latestValue
  ) where

import Prelude
import Data.Array (last) as Array
import Data.Maybe (Maybe(..))
import Effect.Timer (clearTimeout, setTimeout)
import React.Basic.Hooks (useEffect)

data DebouncedInput
  = DebouncedInput
    { allValues :: Array String
    , debounced :: Maybe String
    }

derive instance equalDebounced :: Eq DebouncedInput

empty :: DebouncedInput
empty = DebouncedInput { allValues: [], debounced: Nothing }

init :: String -> DebouncedInput
init initialValue = DebouncedInput { allValues: [ initialValue ], debounced: Just initialValue }

debouncedValue :: DebouncedInput -> Maybe String
debouncedValue (DebouncedInput { debounced }) = debounced

latestValue :: DebouncedInput -> Maybe String
latestValue (DebouncedInput { allValues }) = Array.last allValues

addValue :: String -> DebouncedInput -> DebouncedInput
addValue newValue (DebouncedInput { allValues, debounced }) = DebouncedInput { allValues: (allValues <> [ newValue ]), debounced }

bounce debouncedInput bounceAction =
    useEffect debouncedInput do
      timeoutId <- setTimeout 500 (bounceAction debouncedInput)
      pure (clearTimeout timeoutId)

debounce :: DebouncedInput -> DebouncedInput -> DebouncedInput
debounce (DebouncedInput previous) (DebouncedInput current) =
  if previous.allValues == current.allValues then
    DebouncedInput (current { debounced = Array.last current.allValues })
  else
    DebouncedInput current
