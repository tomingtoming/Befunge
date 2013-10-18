module Befunge.Stack (
  Stack,
  newStack,
  deleteStack,
  pop,
  push
) where

import Foreign

data Stack = Stack {
  setSize    :: Ptr Int,
  setPointer :: Ptr Int,
  setArray   :: Ptr Word8
} deriving Show

newStack :: Int -> IO Stack
newStack size = do
  s <- mallocArray ((sizeOf size) * 2 + size)
  p <- return $ plusPtr s (sizeOf size)
  a <- return $ plusPtr p (sizeOf size)
  poke s size
  poke p 0
  return $ Stack { setSize = s, setPointer = p, setArray = a }

deleteStack :: Stack -> IO ()
deleteStack (Stack s _ _) = free s

pop :: Stack -> IO Word8
pop (Stack _ p a) = do
  p' <- peek p
  if 0 < p'
  then poke p (p'-1) >> peekElemOff a (fromIntegral (p'-1))
  else return 0

push :: Stack -> Word8 -> IO ()
push (Stack s p a) n = do
  s' <- peek s
  p' <- peek p
  if p' < s'
  then poke p (p'+1) >> pokeElemOff a (fromIntegral p') n
  else error "stack overflow"
