module Befunge.Field.Memory where

import Befunge.Field
import Foreign

data MemField = MemField {
  setWidth  :: Ptr Int,
  setHeight :: Ptr Int,
  setArray  :: Ptr Word8
} deriving Show

instance Field MemField where

  deleteField (MemField w _ _) = free w

  get x y (MemField w h a) = do
    w' <- peek w
    h' <- peek h
    peekElemOff a ((mod x w') + (mod y h') * w')

  put x y (MemField w h a) n = do
    w' <- peek w
    h' <- peek h
    pokeElemOff a ((mod x w') + (mod y h') * w') n

newMemField w' h' = do
  p <- mallocArray ((sizeOf w') + (sizeOf h') + (w' * h')) :: IO (Ptr Int)
  newFieldByPtr w' h' p

newFieldByPtr w' h' p = do
  w <- return $ castPtr p
  h <- return $ plusPtr w (sizeOf w')
  a <- return $ plusPtr h (sizeOf h')
  poke w w'
  poke h h'
  return $ MemField { setWidth  = w, setHeight = h, setArray  = a }
