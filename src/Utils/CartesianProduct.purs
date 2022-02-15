module Utils.CartesianProduct
  ( combine
  )
  where

import Data.Traversable (sequence)
import Prelude (class Show)


combine :: forall a . Show a => Array (Array a)  -> Array (Array a)
combine [[]] = [[]]
combine x =  sequence x
  
  
