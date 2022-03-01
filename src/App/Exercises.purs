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
  , accessorByIndex
  , literals
  , comma
  , iterator
  , accessorChaining
  , pipe
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
  , json:
      """
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

accessorByIndex :: Exercise
accessorByIndex =
  { name: "Accessor - by index"
  , description:
      """
When the JSON input is an array (`[ ... ]`), you can pick any of its items like `.[<index>]`.

So, for example, if you need to just get the first pet in `[ "Panda", "Cat", "Monkey"]`, then
the JQ expression `.[0]` would yield `"Panda"`.

**Objective**: pick a food item out of the array.
"""
  , json:
      """
[
  "Gin & Tonic",
  "Bread",
  "Rock"
]
"""
  , solution: [ """ "Bread" """ ]
  }

literals :: Exercise
literals =
  { name: "Literals"
  , description:
      """
Your JQ program can contain any JSON literals.

For example, the JQ expression `42` simply ignores the JSON input and yields `42` as the output.

All JSON types are supported, and you can mix the given JSON input into your literal:

```jq
json input: { "iLove": "bread" }
jq expression: [ 1, 2, . ]

jq output: [ 1, 2, { "iLove": "bread" } ]
```

**Objective**: build the following array of colours: `[ "Green", "White", "Red" ]`.

You could _cheat_ and just type it out, but that would be no fun; try to use the given input as well.
"""
  , json:
      """
{
  "wine": "Red",
  "lime": "Green"
}
"""
  , solution: [ """[ "Green", "White", "Red" ]""" ]
  }

comma :: Exercise
comma =
  { name: "Comma"
  , description:
      """
Something that might initially seem confusing is that JQ outputs a list of JSONs, rather than just one.

We will see how this can be helpful later on, but until then, you should just know that a given JQ expression
can yield more than one JSON. 

The comma operator (`,`) simply outputs its left and right expressions as separate JSON:

```jq
json input: { "ciabatta": "italian", "baguette": "french" }
jq expression: .ciabatta , .baguette

jq output:
  - "italian"
  - "french"
```

Finally, bear in mind that you can chain as many `,` as you like , so the JQ expression `1 , 2 , 3` yields
three JSON outputs.

**Objective**: put the bread type, rating and list of ingredients into separate JSON outputs.
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
  , solution:
      [ """ "Baguette" """
      , "4.5"
      , """["Flour", "Water", "Yeast", "Salt"]"""
      ]
  }

iterator :: Exercise
iterator =
  { name: "Iterator"
  , description:
      """
The iterator (`[]`) lets you output all values in an array or object:

```jq
json input: [1, 2, 3]
jq expression: .[]

jq output:
  - 1
  - 2
  - 3
```

**Objective**: output all colours from the given object.
"""
  , json:
      """
{
  "a": "Green",
  "b": "White",
  "c": "Red"
}
"""
  , solution:
      [ """ "Green" """
      , """ "White" """
      , """ "Red" """
      ]
  }

accessorChaining :: Exercise
accessorChaining =
  { name: "Accessor - chaining"
  , description:
      """
For nested JSON, you can chain accessors (`.<key>`, `.[<index>]`), and you can also chain the iterator (`[]`):

```jq
json input:
  {
    "breads": [
      { "type": "baguette", "ingredients": ["flour",  "water", "salt", "yeast"] },
      { "type": "focaccia", "ingredients": ["flour",  "water", "oil", "salt", "yeast"] }
    ]
  }
jq expression: .breads[1].ingredients[]

jq output:
  - "flour"
  - "water"
  - "oil"
  - "salt"
  - "yeast"
```

**Objective**: pick the colours of the Italian flag and output them individually.
"""
  , json:
      """
{
  "flags": [
    { 
      "nation": "Italy",
      "colours": [ "Green", "White", "Red" ]
    },
    { 
      "nation": "Japan",
      "colours": [ "White", "Red" ]
    }
  ]
}
"""
  , solution:
      [ """ "Green" """
      , """ "White" """
      , """ "Red" """
      ]
  }

pipe :: Exercise
pipe =
  { name: "Pipe"
  , description:
      """
The pipe operator (`|`) lets you sequence JQ expressions: the JSON output from the expression on the left of the
pipe is fed as input to the expression on the right.

If the JQ expression on the left yields more than one JSON output, then the JQ expression on the right runs for each of the
outputs:

```jq
json input:
  {
    "pizzas": [
      { "type": "margherita" },
      { "type": "quattro formaggi" }
    ]
  }
jq expression: .pizzas | .[] | .type

jq output:
  - "margherita"
  - "quattro formaggi"
```

As you can see, you can have as many pipes as you like.

Often you can refactor a JQ expression to not use pipes; for instance, the example above could just be `.pizzas[].type`.
However, pipes can be very useful in breaking down a problem into smaller steps and make a program easier to reason about.

**Objective**: return Water and Yeast as separate outputs, using pipe, accessors and comma.
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
  , solution:
      [ """ "Water" """
      , """ "Yeast" """
      ]
  }
