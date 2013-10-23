module Befunge.MMap.Field (
  MMapField,
  newMMapField,
  srcMMapField,
  restoreMMapField
) where

import Befunge.Field
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as C8
import Foreign
import System.IO.MMap

data MMapField = MMapField {
  setWidth  :: Ptr Int,
  setHeight :: Ptr Int,
  setArray  :: Ptr Word8
} deriving Show

instance Field MMapField where

  deleteField (MMapField w h _) = do
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

newMMapField :: Int -> Int -> FilePath -> IO MMapField
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

restoreMMapField :: FilePath -> IO MMapField
restoreMMapField path = do
  (p,rawSize,_,_) <- mmapFilePtr path ReadWrite Nothing
  w <- return $ castPtr p
  h <- return $ plusPtr w (sizeOf (0::Int))
  a <- return $ plusPtr h (sizeOf (0::Int))
  w' <- peek w
  h' <- peek h
  let size = (sizeOf w') + (sizeOf h') + (w' * h')
  _ <- if rawSize == size then return () else error "MMap Fail"
  return $ MMapField { setWidth  = w, setHeight = h, setArray  = a }

srcMMapField :: FilePath -> FilePath -> IO MMapField
srcMMapField src path = do
  ls <- fmap C8.lines $ C8.readFile src
  let
    w'    = foldl (\n bs -> max n $ C8.length bs) 0 ls
    h'    = length ls
    size = (sizeOf w') + (sizeOf h') + (w' * h')
  (p,rawSize,_,_) <- mmapFilePtr path ReadWriteEx $ Just (0,size)
  _ <- if rawSize == size then return () else error "MMap Fail"
  w <- return $ castPtr p
  h <- return $ plusPtr w (sizeOf (0::Int))
  a <- return $ plusPtr h (sizeOf (0::Int))
  poke w w'
  poke h h'
  mapM_ (\(n,bs) -> pokeArray (plusPtr a ((mod n h') * w')) $ Bs.unpack bs) $ zip [0..] ls
  return $ MMapField { setWidth = w, setHeight = h, setArray  = a }
