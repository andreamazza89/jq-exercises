module App.Exercises
  ( Exercise
  , all
  , first
  , next
  ) where

import Prelude ((#), (==), (>>>), flip, not)
import Data.Array as Array
import Data.Maybe (Maybe)

type Exercise
  = { name :: String
    , description :: String
    , json :: String
    , solution :: Array String
    }

all :: Array Exercise
all =
  [ identity
  , accessorByKey
  ]

first :: Exercise
first = identity

next :: Exercise -> Maybe Exercise
next exercise =
  Array.dropWhile (isSameExercise exercise >>> not) all
    # flip Array.index 1

-- we should probably 'upgrade' Exercise to a NewType and then implement EQ, though at the moment -- this is light touch/simple data bag, so will wait and see if necessary.
isSameExercise :: Exercise -> Exercise -> Boolean
isSameExercise l r = l.name == r.name

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
  , json: """
{
  "iLove": "bread"
}
"""
  , solution: [ """{"iLove": "bread"}""" ]
  }

accessorByKey :: Exercise
accessorByKey =
  { name: "Accessor - by key"
  , description:
      """
When the JSON input is an object (`{ ... }`), you can pick any of its keys like `.<key-name>`
to get that key's value.

So, for example, if you need to just get the name out of `{ "age": 33, "name": "Giorgio" }`, then
the JQ expression `.name` would yield `"Giorgio"`.

_(`."name"` and `.["name"]` are equivalent forms to the above and useful if the key you need 
name has special characters, however these are not yet supported on this site)_

**Objective**: extract the ingredients to make bread.
"""
  , json:
      """
{
  "type": "Baguette",
  "rating": 4.5,
  "ingredients": ["Flour", "Water", "Yeast", "Salt"],
  "crispiness": ":star-struck:"
}
"""
  , solution: [ """["Flour", "Water", "Yeast", "Salt"]""" ]
  }
