module Befunge.Field where

import Foreign

class Field f where
  deleteField :: f -> IO ()
  get         :: Int -> Int -> f -> IO Word8
  put         :: Int -> Int -> f -> Word8 -> IO ()


