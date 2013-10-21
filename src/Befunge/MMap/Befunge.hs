module Befunge.MMap.Befunge (
  MMapBefunge,
  newMMapBefunge
) where

import Befunge
import Befunge.Field
import Befunge.Stack
import Foreign
import System.IO.MMap

data MMapBefunge = MMapBefunge {
  setX         :: Ptr Int,
  setY         :: Ptr Int,
  setDirection :: Ptr Word8,
  setStack     :: MemStack
}

instance Befunge MMapBefunge where

  step bf@(MMapBefunge x y d s) field = do
    x' <- peek x
    y' <- peek y
    w' <- get x' y' field
    return True

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
  return $ MMapBefunge { setX = x, setY = y, setDirection = d, setStack = s }

