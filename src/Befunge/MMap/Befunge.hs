module Befunge.MMap.Befunge (
  MMapBefunge,
  newMMapBefunge
) where

import Befunge
import Befunge.Stack
import Foreign
import System.IO.MMap

data MMapBefunge = MMapBefunge {
  xPtr         :: Ptr Int,
  yPtr         :: Ptr Int,
  dPtr         :: Ptr Word8,
  setStack     :: MemStack
}

instance Befunge MMapBefunge where

  getX (MMapBefunge x _ _ _)   = peek x

  setX (MMapBefunge x _ _ _) n = poke x n

  getY (MMapBefunge _ y _ _)   = peek y

  setY (MMapBefunge _ y _ _) n = poke y n

  getDirection (MMapBefunge _ _ d _)   = peek d

  setDirection (MMapBefunge _ _ d _) n = poke d n

  popStack (MMapBefunge _ _ _ s)    = pop s

  pushStack (MMapBefunge _ _ _ s) n = push s n

newMMapBefunge :: Int -> Int -> Word8 -> Int -> FilePath -> IO MMapBefunge
newMMapBefunge x' y' d' l path = do
  let size = (sizeOf x') + (sizeOf y') + (sizeOf d') + l
  (m,rawSize,_,_) <- mmapFilePtr path ReadWriteEx $ Just (0,size)
  _ <- return $ if rawSize == size then () else error "MMap Fail"
  x <- return $ castPtr m
  y <- return $ plusPtr x (sizeOf x')
  d <- return $ plusPtr y (sizeOf y')
  s <- newStackByPtr l $ plusPtr d (sizeOf d')
  poke x x'
  poke y y'
  poke d d'
  return $ MMapBefunge { xPtr = x, yPtr = y, dPtr = d, setStack = s }
