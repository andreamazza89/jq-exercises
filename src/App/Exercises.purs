module App.Exercises
  ( Exercise
  , all
  , first
  , next
  )
  where

import Prelude ((#), (==), (>>>), flip, not)
import Data.Array as Array
import Data.Maybe (Maybe(..))

type Exercise
  = { name :: String
    , description :: String
    , json :: String
    , solution :: Array String
    }

all :: Array Exercise
all = [ identity ]

first :: Exercise
first = identity

next :: Exercise -> Maybe Exercise
next exercise =
  Array.dropWhile (isSameExercise exercise >>> not) all
    # flip Array.index 1

isSameExercise :: Exercise -> Exercise -> Boolean
isSameExercise l r = -- we should probably 'upgrade' Exercise to a NewType and then implement EQ, though at the moment
  -- this is light touch/simple data bag, so will wait and see if necessary.
  l.name == r.name

identity :: Exercise
identity =
  { name: "Identity operator"
  , description:
      """
The identity operator (`.`) is the simplest of them all: it just returns the input JSON.

So, for example, running the JQ expression `.` against JSON input `[ "ciao" ]`, simply yields `[ "ciao" ]`.

At first, this might seem like a pointless operator as it leaves the input unchanged, however you'll see
how important it is in future exercises.

**Objective**: your JQ expression should return the input unchanged.
"""
  , json: "{\"iLove\": \"bread\"}"
  , solution: [ "{\"iLove\": \"bread\"}" ]
  }
