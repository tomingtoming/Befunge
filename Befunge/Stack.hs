module Befunge.Stack where

import Foreign (Word8)

class Stack s where
  deleteStack :: s -> IO ()
  pop         :: s -> IO Word8
  push        :: s -> Word8 -> IO ()
