module Befunge where

import Befunge.Field
import Befunge.Stack
import Foreign

class Befunge b where
  step         :: Field f => b -> f -> IO Bool
  getX         :: b -> IO Int
  setX         :: b -> Int -> IO ()
  getY         :: b -> IO Int
  setY         :: b -> Int -> IO ()
  getDirection :: b -> IO Word8
  setDirection :: b -> Word8 -> IO ()
  popStack     :: b -> IO Word8
  pushStack    :: b -> Word8 -> IO ()

move :: Befunge b => b -> IO ()
move bf = do
  d <- getDirection bf
  if mod d 2 == 1
  then getX bf >>= \x -> setX bf $ x + 2 - (fromIntegral d)
  else getY bf >>= \y -> setY bf $ y - 1 + (fromIntegral d)

steps :: (Befunge b, Field f) => b -> f -> IO ()
steps bf f = do
  alive <- step bf f
  if alive then steps bf f
           else return ()
