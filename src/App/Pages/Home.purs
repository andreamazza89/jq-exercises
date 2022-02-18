module App.Pages.Home where

import Prelude

import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

type HomeProps = Unit

mkHome :: Component HomeProps
mkHome = do
  component "Home" \_props -> React.do
    pure $ DOM.div
      { children:
          [ DOM.h1_ [ DOM.text "Home" ]
          ]
      }