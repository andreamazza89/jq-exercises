module App.Exercises
  ( Exercise
  , all
  , first
  ) where

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
