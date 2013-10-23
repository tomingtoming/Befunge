module Befunge where

import Befunge.Field
import Befunge.Stack

class Befunge b where
  step     :: Field f => b -> f -> IO Bool
  getX     :: b -> IO Int
  setX     :: b -> Int -> IO ()
  getY     :: b -> IO Int
  setY     :: b -> Int -> IO ()

steps :: (Befunge b, Field f) => b -> f -> IO ()
steps bf f = do
  alive <- step bf f
  if alive then steps bf f
           else return ()
