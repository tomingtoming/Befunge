module Befunge where

import Befunge.Field
import Control.Monad (liftM2)
import Data.Char (chr, ord)
import qualified Data.Map as Map
import Foreign
import System.Random (randomIO)

class Befunge b where
  fire         :: Field f => b -> f -> IO ()
  spawn        :: Int -> Int -> Word8 -> b -> IO b
  getX         :: b -> IO Int
  setX         :: b -> Int -> IO ()
  getY         :: b -> IO Int
  setY         :: b -> Int -> IO ()
  getDirection :: b -> IO Word8
  setDirection :: b -> Word8 -> IO ()
  popStack     :: b -> IO Word8
  pushStack    :: b -> Word8 -> IO ()
  delBefunge   :: b -> IO ()

c2w :: Char -> Word8
c2w = fromIntegral . ord

w2c :: Word8 -> Char
w2c = chr . fromIntegral

move :: Befunge b => b -> IO ()
move bf = do
  d <- getDirection bf
  if mod d 2 == 1
  then getX bf >>= \x -> setX bf $ x + 2 - (fromIntegral d)
  else getY bf >>= \y -> setY bf $ y - 1 + (fromIntegral d)

step :: (Befunge b, Field f) => b -> f -> IO Bool
step bf field = do
  x <- getX bf
  y <- getY bf
  w <- get x y field
  alive <- case Map.lookup w instructions of
    Just func -> func bf field
    Nothing   -> return True
  move bf
  return alive

steps :: (Befunge b, Field f) => b -> f -> IO ()
steps bf f = do
  alive <- step bf f
  if alive then steps bf f
           else return ()

literals :: (Befunge b, Field f) => b -> f -> IO Bool
literals bf field = do
  move bf
  x <- getX bf
  y <- getY bf
  w <- get x y field
  if w == c2w '"'
  then return True
  else pushStack bf w >> literals bf field

instructions :: (Befunge b, Field f) => Map.Map Word8 (b -> f -> IO Bool)
instructions = Map.fromList $
  [ (c2w '#', \bf _ -> move bf >> return True)
  , (c2w '^', \bf _ -> setDirection bf 0 >> return True)
  , (c2w '>', \bf _ -> setDirection bf 1 >> return True)
  , (c2w 'v', \bf _ -> setDirection bf 2 >> return True)
  , (c2w '<', \bf _ -> setDirection bf 3 >> return True)
  , (c2w '|', \bf _ -> popStack bf >>= \v -> setDirection bf (if v == 0 then 2 else 0) >> return True)
  , (c2w '_', \bf _ -> popStack bf >>= \v -> setDirection bf (if v == 0 then 1 else 3) >> return True)
  , (c2w '?', \bf _ -> randomIO >>= \r -> setDirection bf (mod r 4) >> return True)
  , (c2w ' ', \_ _  -> return True)
  , (c2w '@', \_ _  -> return False)

  {- Literal Instructions -}
  , (c2w '0', \bf _ -> pushStack bf 0 >> return True)
  , (c2w '1', \bf _ -> pushStack bf 1 >> return True)
  , (c2w '2', \bf _ -> pushStack bf 2 >> return True)
  , (c2w '3', \bf _ -> pushStack bf 3 >> return True)
  , (c2w '4', \bf _ -> pushStack bf 4 >> return True)
  , (c2w '5', \bf _ -> pushStack bf 5 >> return True)
  , (c2w '6', \bf _ -> pushStack bf 6 >> return True)
  , (c2w '7', \bf _ -> pushStack bf 7 >> return True)
  , (c2w '8', \bf _ -> pushStack bf 8 >> return True)
  , (c2w '9', \bf _ -> pushStack bf 9 >> return True)
  , (c2w '"', literals)

  {- I/O Instructions -}
  , (c2w ',', \bf _ -> popStack bf >>= \w -> putChar (w2c w) >> return True)
  , (c2w '.', \bf _ -> popStack bf >>= \w -> putStr ((show w) ++ " ") >> return True)

  {- Math Instructions -}
  , (c2w '+', \bf _ -> popStack bf >>= \y -> popStack bf >>= \x -> pushStack bf (x + y) >> return True)
  , (c2w '-', \bf _ -> popStack bf >>= \y -> popStack bf >>= \x -> pushStack bf (x - y) >> return True)
  , (c2w '*', \bf _ -> popStack bf >>= \y -> popStack bf >>= \x -> pushStack bf (x * y) >> return True)
  , (c2w '/', \bf _ -> popStack bf >>= \y -> popStack bf >>= \x -> pushStack bf (x `div` y) >> return True)
  , (c2w '%', \bf _ -> popStack bf >>= \y -> popStack bf >>= \x -> pushStack bf (x `mod` y) >> return True)
  , (c2w '`', \bf _ -> popStack bf >>= \y -> popStack bf >>= \x -> pushStack bf (if x > y then 1 else 0) >> return True)
  , (c2w '!', \bf _ -> popStack bf >>= \v -> pushStack bf (if v == 0 then 1 else 0) >> return True)

  {- Stack Instructions -}
  , (c2w ':', \bf _ -> popStack bf >>= \v -> pushStack bf v >> pushStack bf v >> return True)
  , (c2w '\\',\bf _ -> popStack bf >>= \y -> popStack bf >>= \x -> pushStack bf x >> pushStack bf y >> return True)
  , (c2w '$', \bf _ -> popStack bf >> return True)

  {- Memory Instructions -}
  , (c2w 'g', \bf f -> liftM2 (+) (getY bf) (fmap fromIntegral $ popStack bf) >>= \y -> liftM2 (+) (getX bf) (fmap fromIntegral $ popStack bf) >>= \x -> get x y f >>= \v -> pushStack bf v >> return True)
  , (c2w 'p', \bf f -> liftM2 (+) (getY bf) (fmap fromIntegral $ popStack bf) >>= \y -> liftM2 (+) (getX bf) (fmap fromIntegral $ popStack bf) >>= \x -> popStack bf >>= \v -> put x y f v >> return True)

  {- Thread Instructions -}
  , (c2w 'w', \bf f -> liftM2 (+) (getY bf) (fmap fromIntegral $ popStack bf) >>= \y -> liftM2 (+) (getX bf) (fmap fromIntegral $ popStack bf) >>= \x -> popStack bf >>= \d -> spawn x y d bf >>= \bf' -> fire bf' f >> return True)
  ]
