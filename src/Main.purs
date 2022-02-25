module Main where

import Prelude

import App (mkApp)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  body <- body =<< document =<< window

  case body of
    Nothing -> throw "Body not found."
    Just body' -> do
      app <- mkApp
      render (app unit) (toElement body')
