module Befunge.MMap.Stack (
  MMapStack,
  newMMapStack
) where

import Befunge.Stack
import Foreign
import System.IO.MMap

data MMapStack = MMapStack {
  setSize    :: Ptr Int,
  setPointer :: Ptr Int,
  setArray   :: Ptr Word8
} deriving Show

instance Stack MMapStack where

  deleteStack (MMapStack s _ _) = do
    s' <- peek s
    munmapFilePtr s $ (sizeOf s') + (sizeOf s') + s'

  pop (MMapStack _ p a) = do
    p' <- peek p
    if 0 < p'
    then poke p (p'-1) >> peekElemOff a (fromIntegral (p'-1))
    else return 0

  push (MMapStack s p a) n = do
    s' <- peek s
    p' <- peek p
    if p' < s'
    then poke p (p'+1) >> pokeElemOff a (fromIntegral p') n
    else error "stack overflow"

newMMapStack :: Int -> FilePath -> IO MMapStack
newMMapStack h path = do
  let size = (sizeOf h) * 2 + h
  (m,rawSize,_,_) <- mmapFilePtr path ReadWriteEx $ Just (0,size)
  _ <- return $ if rawSize == size then () else error "MMap Fail"
  s <- return $ castPtr m
  p <- return $ plusPtr s (sizeOf h)
  a <- return $ plusPtr p (sizeOf h)
  poke s size
  poke p 0
  return $ MMapStack { setSize = s, setPointer = p, setArray = a }
