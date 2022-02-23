module Helpers.Expression
  ( (||)
  , (~)
  , accessAllItems
  , accessByIndex
  , accessByKeyNames
  , accessor
  , allItems
  , atIndex
  , atKey
  , comma
  , constructArray
  , constructEmptyArray
  , constructEmptyObject
  , constructObject
  , identity
  , literal
  , pipe
  , toNonEmpty
  )
  where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray, singleton) as NE
import Data.Functor (map)
import Data.Maybe (Maybe(..), fromMaybe)
import Expression (Expression(..), Over(..), Target(..), KeyValuePair)
import Json (Json)
import Prelude (($), (>>>))

-- Helpers to build Expressions
accessByKeyNames :: Array String -> Expression
accessByKeyNames = map Key >>> accessor

accessByIndex :: Array Int -> Expression
accessByIndex = map AtIndex >>> accessor

constructArray :: Expression -> Expression
constructArray = Just >>> ArrayConstructor

constructEmptyArray :: Expression
constructEmptyArray = ArrayConstructor Nothing

constructObject :: Array (KeyValuePair) -> Expression
constructObject =  ObjectConstructor

constructEmptyObject :: Expression
constructEmptyObject = constructObject []

literal :: Json -> Expression
literal = Literal

accessAllItems :: Expression
accessAllItems = Accessor Input (NE.singleton Each)

accessor ∷ Array Target → Expression
accessor = toNonEmpty >>> Accessor Input

toNonEmpty :: Array Target -> NonEmptyArray Target
toNonEmpty = NE.fromArray >>> fromMaybe (NE.singleton $ Key "ERROR - empty path")

atKey :: String -> Target
atKey = Key

atIndex :: Int -> Target
atIndex = AtIndex

allItems :: Target
allItems = Each

identity :: Expression
identity = Identity

pipe :: Expression -> Expression -> Expression
pipe = Pipe

infixl 6 pipe as ||

comma :: Expression -> Expression -> Expression
comma = Comma

infixl 5 comma as ~