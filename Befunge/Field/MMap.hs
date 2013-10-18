module Befunge.Field.MMap where

import Befunge.Field
import Foreign
import System.IO.MMap

data MMapField = MMapField {
  setWidth  :: Ptr Int,
  setHeight :: Ptr Int,
  setArray  :: Ptr Word8
} deriving Show

instance Field MMapField where

  deleteField (MMapField w h a) = do
    w'   <- peek w
    h'   <- peek h
    size <- return $ (sizeOf w') + (sizeOf h') + (w' * h')
    munmapFilePtr w size

  get x y (MMapField w h a) = do
    w' <- peek w
    h' <- peek h
    peekElemOff a ((mod x w') + (mod y h') * w')

  put x y (MMapField w h a) n = do
    w' <- peek w
    h' <- peek h
    pokeElemOff a ((mod x w') + (mod y h') * w') n

newMMapField w' h' path = do
  let size = (sizeOf w') + (sizeOf h') + (w' * h')
  (p,rawSize,_,_) <- mmapFilePtr path ReadWriteEx $ Just (0,size)
  _ <- return $ if rawSize == size then () else error "MMap Fail"
  w <- return $ castPtr p
  h <- return $ plusPtr w (sizeOf w')
  a <- return $ plusPtr h (sizeOf h')
  poke w w'
  poke h h'
  return $ MMapField { setWidth  = w, setHeight = h, setArray  = a }
