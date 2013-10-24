module Befunge.Stack (
  Stack(..),
  MemStack,
  newMemStack,
  newStackByPtr
) where

import Foreign

class Stack s where
  deleteStack :: s -> IO ()
  pop         :: s -> IO Word8
  push        :: s -> Word8 -> IO ()
  depth       :: s -> IO Int

data MemStack = MemStack {
  setSize    :: Ptr Int,
  setPointer :: Ptr Int,
  setArray   :: Ptr Word8
} deriving Show

instance Stack MemStack where

  deleteStack (MemStack s _ _) = free s

  pop (MemStack _ p a) = do
    p' <- peek p
    if 0 < p'
    then poke p (p'-1) >> peekElemOff a (fromIntegral (p'-1))
    else return 0

  push (MemStack s p a) n = do
    s' <- peek s
    p' <- peek p
    if p' < s'
    then poke p (p'+1) >> pokeElemOff a (fromIntegral p') n
    else error "stack overflow"

  depth (MemStack s _ _) = peek s

newMemStack :: Int -> IO MemStack
newMemStack size = do
  headPtr <- mallocArray ((sizeOf size) * 2 + size) :: IO (Ptr Int)
  newStackByPtr size headPtr

newStackByPtr :: Int -> Ptr a -> IO MemStack
newStackByPtr size headPtr = do
  s <- return $ castPtr headPtr
  p <- return $ plusPtr s (sizeOf size)
  a <- return $ plusPtr p (sizeOf size)
  poke s size
  poke p 0
  return $ MemStack { setSize = s, setPointer = p, setArray = a }
