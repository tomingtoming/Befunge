module Befunge where

import Befunge.Field

class Befunge b where
  step :: Field f => b -> f -> IO ()
