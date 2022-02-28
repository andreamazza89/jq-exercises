module WebComponents.Markdown (build) where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import React.Basic.DOM (unsafeCreateDOMComponent)
import React.Basic.Hooks (JSX, element)
import Unsafe.Coerce (unsafeCoerce)

build :: String -> JSX
build content =
  element markdownComponent { content }
  where
    markdownComponent =
      unsafeCreateDOMComponent "mark-down"
        # unsafePerformEffect
        # unsafeCoerce