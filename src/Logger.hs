module Logger (
  putStrPar,
  putStrLnPar,
  printPar
) where

import Control.Concurrent
import GHC.IO (unsafePerformIO)

{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

putStrLnPar :: String -> IO ()
putStrLnPar x = do
  takeMVar lock
  putStrLn x
  putMVar lock ()

putStrPar :: String -> IO ()
putStrPar x = do
  takeMVar lock
  putStr x
  putMVar lock ()

printPar :: Show a => a -> IO ()
printPar x = do
  takeMVar lock
  print x
  putMVar lock ()