module Befunge.MMap.Befunge (
  MMapBefunge,
  newMMapBefunge
) where

import Befunge
import Befunge.ForkJoin
import Befunge.Stack
import Control.Exception (try, SomeException)
import Foreign
import System.Directory (removeFile)
import System.IO.MMap
import System.Random

data MMapBefunge = MMapBefunge {
  xPtr         :: Ptr Int,
  yPtr         :: Ptr Int,
  dPtr         :: Ptr Word8,
  setStack     :: MemStack,
  setForkSet   :: ForkSet,
  setFilePath  :: FilePath,
  rawSize      :: Int
}

instance Befunge MMapBefunge where

  fire bf f = do
    let fs = setForkSet bf
    _ <- forkWith fs ((try (steps bf f) :: IO (Either SomeException ())) >> delBefunge bf)
    return ()

  spawn x y d bf = do
    rn <- randomRIO (0,1000000000000000000000000000000) :: IO Integer
    newMMapBefunge x y d 1024 ((++) "befunge." $ show rn) (setForkSet bf)

  getX bf = peek (xPtr bf)

  setX bf n = poke (xPtr bf) n

  getY bf = peek (yPtr bf)

  setY bf n = poke (yPtr bf) n

  getDirection bf = peek (dPtr bf)

  setDirection bf n = poke (dPtr bf) n

  popStack bf = pop (setStack bf)

  pushStack bf n = push (setStack bf) n

  delBefunge bf = do
    munmapFilePtr (xPtr bf) (rawSize bf)
    removeFile $ setFilePath bf

newMMapBefunge :: Int -> Int -> Word8 -> Int -> FilePath -> ForkSet -> IO MMapBefunge
newMMapBefunge x' y' d' l path fs = do
  let size = (sizeOf x') + (sizeOf y') + (sizeOf d') + l
  (m,rs,_,_) <- mmapFilePtr path ReadWriteEx $ Just (0,size)
  _ <- return $ if rs == size then () else error "MMap Fail"
  x <- return $ castPtr m
  y <- return $ plusPtr x (sizeOf x')
  d <- return $ plusPtr y (sizeOf y')
  s <- newStackByPtr l $ plusPtr d (sizeOf d')
  poke x x'
  poke y y'
  poke d d'
  return $ MMapBefunge { xPtr = x, yPtr = y, dPtr = d, setStack = s, setForkSet = fs, setFilePath = path, rawSize = rs }
