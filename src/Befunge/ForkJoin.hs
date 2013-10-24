module Befunge.ForkJoin where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.Set

newtype ForkSet = ForkSet (TVar (Set ThreadId))

newForkSet :: IO ForkSet
newForkSet = fmap ForkSet $ newTVarIO empty

forkWith :: ForkSet -> IO () -> IO ThreadId
forkWith (ForkSet tv) io = do
  th <- forkIO $ do
    th <- myThreadId
    atomically $ readTVar tv >>= \s -> if member th s then return () else retry
    _ <- try io :: IO (Either SomeException ())
    atomically $ modifyTVar tv (delete th)
  atomically $ modifyTVar tv (insert th)
  return th

joinWith :: ForkSet -> ThreadId -> IO ()
joinWith (ForkSet tv) th = atomically $ do
  s <- readTVar tv
  if member th s
  then return ()
  else retry

joinAllWith :: ForkSet -> IO ()
joinAllWith (ForkSet tv) = atomically $ do
  s <- readTVar tv
  if Data.Set.null s
  then return ()
  else retry
