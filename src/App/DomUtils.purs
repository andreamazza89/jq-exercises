module App.DomUtils
  ( button
  , container
  , errorMessage
  , h2
  , inputChanged
  , row
  , showJson
  , showJsons
  , successMessage
  ) where

import Prelude
import Data.Maybe (fromMaybe)
import Effect (Effect)
import React.Basic.DOM (css)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture, targetValue)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (JSX)
import WebComponents.Markdown as Markdown

button :: String -> EventHandler -> JSX
button text onClick =
  DOM.button
    { children: [ DOM.text text ]
    , className: "outline"
    , onClick
    }

container :: Array JSX -> JSX
container children =
  DOM.div
    { className: "container"
    , children
    }

inputChanged :: forall action. (action -> Effect Unit) -> (String -> action) -> EventHandler
inputChanged dispatch buildAction = capture targetValue (fromMaybe "does this ever happen?" >>> buildAction >>> dispatch)

h2 :: String -> JSX
h2 content = DOM.h2_ [ DOM.text content ]

errorMessage :: String -> JSX
errorMessage = textWithColor "#fd5050"

successMessage :: String -> JSX
successMessage = textWithColor "#21c782"

row :: Array JSX -> JSX
row items = DOM.div { className: "grid", children: items }

textWithColor :: String -> String -> JSX
textWithColor color text = DOM.h4 { children: [ DOM.text text ], style: (css { color }) }

showJsons :: String -> Array String -> JSX
showJsons label jsons =
  DOM.article_
    [ DOM.header_ [ DOM.text (label <> ":") ]
    , DOM.div_ $ map showJson jsons
    ]

showJson :: String -> JSX
showJson =
  toJsonCodeBlock >>> Markdown.build 

toJsonCodeBlock :: String -> String
toJsonCodeBlock jsonString = "```json\n" <> jsonString <> "\n```"
