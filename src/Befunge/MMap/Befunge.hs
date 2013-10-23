module Befunge.MMap.Befunge (
  MMapBefunge,
  newMMapBefunge
) where

import Befunge
import Befunge.Field
import Befunge.Stack
import Data.Char (chr, ord)
import qualified Data.Map as Map
import Foreign
import System.IO.MMap
import System.Random (randomIO)

data MMapBefunge = MMapBefunge {
  xPtr         :: Ptr Int,
  yPtr         :: Ptr Int,
  dPtr         :: Ptr Word8,
  setStack     :: MemStack
}

instance Befunge MMapBefunge where

  step bf@(MMapBefunge _ _ _ _) field = do
    x <- getX bf
    y <- getY bf
    w <- get x y field
    alive <- case Map.lookup w instructions of
      Just func -> func bf field
      Nothing   -> return True
    move bf
    return alive

  getX (MMapBefunge x _ _ _)   = peek x

  setX (MMapBefunge x _ _ _) n = poke x n

  getY (MMapBefunge _ y _ _)   = peek y

  setY (MMapBefunge _ y _ _) n = poke y n

  getDirection (MMapBefunge _ _ d _)   = peek d

  setDirection (MMapBefunge _ _ d _) n = poke d n

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

c2w :: Char -> Word8
c2w = fromIntegral . ord

w2c :: Word8 -> Char
w2c = chr . fromIntegral

instructions :: Field f => Map.Map Word8 (MMapBefunge -> f -> IO Bool)
instructions = Map.fromList $
  [ (c2w '#', \bf _                    -> move bf >> return True)
  , (c2w '^', \(MMapBefunge _ _ d _) _ -> poke d 0 >> return True)
  , (c2w '>', \(MMapBefunge _ _ d _) _ -> poke d 1 >> return True)
  , (c2w 'v', \(MMapBefunge _ _ d _) _ -> poke d 2 >> return True)
  , (c2w '<', \(MMapBefunge _ _ d _) _ -> poke d 3 >> return True)
  , (c2w '|', \(MMapBefunge _ _ d s) _ -> pop s >>= \v -> poke d (if v == 0 then 2 else 0) >> return True)
  , (c2w '_', \(MMapBefunge _ _ d s) _ -> pop s >>= \v -> poke d (if v == 0 then 1 else 3) >> return True)
  , (c2w '?', \(MMapBefunge _ _ d s) _ -> randomIO >>= \r -> poke d (mod r 4) >> return True)
  , (c2w ' ', \(MMapBefunge _ _ d s) _ -> return True)
  , (c2w '@', \(MMapBefunge _ _ d s) _ -> return False)

  {- Literal Instructions -}
  , (c2w '0', \(MMapBefunge _ _ _ s) _ -> push s 0 >> return True)
  , (c2w '1', \(MMapBefunge _ _ _ s) _ -> push s 1 >> return True)
  , (c2w '2', \(MMapBefunge _ _ _ s) _ -> push s 2 >> return True)
  , (c2w '3', \(MMapBefunge _ _ _ s) _ -> push s 3 >> return True)
  , (c2w '4', \(MMapBefunge _ _ _ s) _ -> push s 4 >> return True)
  , (c2w '5', \(MMapBefunge _ _ _ s) _ -> push s 5 >> return True)
  , (c2w '6', \(MMapBefunge _ _ _ s) _ -> push s 6 >> return True)
  , (c2w '7', \(MMapBefunge _ _ _ s) _ -> push s 7 >> return True)
  , (c2w '8', \(MMapBefunge _ _ _ s) _ -> push s 8 >> return True)
  , (c2w '9', \(MMapBefunge _ _ _ s) _ -> push s 9 >> return True)
  , (c2w '"', literals)

  {- I/O Instructions -}
  , (c2w ',', \(MMapBefunge _ _ _ s) _ -> pop s >>= \w -> putChar (w2c w) >> return True)
  , (c2w '.', \(MMapBefunge _ _ _ s) _ -> pop s >>= \w -> putStr ((show w) ++ " ") >> return True)

  {- Math Instructions -}
  , (c2w '+', \(MMapBefunge _ _ _ s) _ -> pop s >>= \y -> pop s >>= \x -> push s (x + y) >> return True)
  , (c2w '-', \(MMapBefunge _ _ _ s) _ -> pop s >>= \y -> pop s >>= \x -> push s (x - y) >> return True)
  , (c2w '*', \(MMapBefunge _ _ _ s) _ -> pop s >>= \y -> pop s >>= \x -> push s (x * y) >> return True)
  , (c2w '/', \(MMapBefunge _ _ _ s) _ -> pop s >>= \y -> pop s >>= \x -> push s (x `div` y) >> return True)
  , (c2w '%', \(MMapBefunge _ _ _ s) _ -> pop s >>= \y -> pop s >>= \x -> push s (x `mod` y) >> return True)
  , (c2w '`', \(MMapBefunge _ _ _ s) _ -> pop s >>= \y -> pop s >>= \x -> push s (if x > y then 1 else 0) >> return True)
  , (c2w '!', \(MMapBefunge _ _ _ s) _ -> pop s >>= \v -> push s (if v == 0 then 1 else 0) >> return True)

  {- Stack Instructions -}
  , (c2w ':', \(MMapBefunge _ _ _ s) _ -> pop s >>= \v -> push s v >> push s v >> return True)
  , (c2w '\\',\(MMapBefunge _ _ _ s) _ -> pop s >>= \y -> pop s >>= \x -> push s x >> push s y >> return True)
  , (c2w '$', \(MMapBefunge _ _ _ s) _ -> pop s >> return True)

  {- Memory Instructions -}
  , (c2w 'g', \(MMapBefunge _ _ _ s) f -> pop s >>= \y -> pop s >>= \x -> get (fromIntegral x) (fromIntegral y) f >>= \v -> push s v >> return True)
  , (c2w 'p', \(MMapBefunge _ _ _ s) f -> pop s >>= \y -> pop s >>= \x -> pop s >>= \v -> put (fromIntegral x) (fromIntegral y) f v >> return True)
  ]

literals :: Field f => MMapBefunge -> f -> IO Bool
literals bf field = do
  move bf
  x <- getX bf
  y <- getY bf
  w <- get x y field
  if w == c2w '"'
  then return True
  else push (setStack bf) w >> literals bf field
