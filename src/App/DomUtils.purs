module App.DomUtils
  ( h2
  , errorMessage
  , inputChanged
  , showJson
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

inputChanged :: forall action. (action -> Effect Unit) -> (String -> action) -> EventHandler
inputChanged dispatch buildAction = capture targetValue (fromMaybe "does this ever happen?" >>> buildAction >>> dispatch)

h2 :: String -> JSX
h2 content = DOM.h2_ [ DOM.text content ]

errorMessage :: String -> JSX
errorMessage = textWithColor "#fd5050"

successMessage :: String -> JSX
successMessage = textWithColor "#21c782"

textWithColor :: String -> String -> JSX
textWithColor color text = DOM.h4 { children: [ DOM.text text ], style: (css { color }) }

showJson :: String -> Array String -> JSX
showJson label json =
  DOM.article_
    [ DOM.header_ [ DOM.text (label <> ":") ]
    , DOM.ul_ $ map (\j -> DOM.li_ [ DOM.text j ]) json
    ]
