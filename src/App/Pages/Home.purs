module App.Pages.Home where

import Prelude

import Data.Maybe (maybe)
import JQ as JQ
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (capture, targetValue)
import React.Basic.Hooks (Component, component, useState, (/\))
import React.Basic.Hooks as React

type HomeProps = Unit

mkHome :: Component HomeProps
mkHome = do
  component "Home" \_props -> React.do

    counter /\ setCounter <- useState ({input: "{\"foo\": 42}", exp: ".foo"})

    pure $ DOM.div
      { children:
          [ DOM.h1_ [ DOM.text "Jq - exercises" ]

          , DOM.textarea {
              value: counter.input,
              onChange: capture targetValue (maybe (setCounter (identity)) (\a -> setCounter (\prev -> {input: a, exp: prev.exp})))
            }

          , DOM.textarea {
              value: counter.exp,
              onChange: capture targetValue (maybe (setCounter (identity)) (\a -> setCounter (\prev -> {input: prev.input, exp: a})))
            }

          , DOM.p_ (maybe ([DOM.text "no result yet"]) (map DOM.text) (JQ.run counter.input counter.exp ))
          ]
      }