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
all = [ first ]

first :: Exercise
first =
  { name: "Sample exercise"
  , description: """
  You *can* **definitely** use Markdown `here`.
  """
  , json: "{\"foo\": [42, 43]}"
  , solution: [ "42.0", "43.0" ]
  }
