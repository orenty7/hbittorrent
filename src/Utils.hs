{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils (
  createUdp,
  withUdp,
  createTcp,
  withTcp,
  orFail,
  thenFail,
  convert,
  timeout,
  TimeoutException,
) where

import qualified Network.Socket as Socket

import Control.Concurrent
import Control.Exception

data TimeoutException = TimeoutException deriving (Show, Exception)

createUdp :: IO Socket.Socket
createUdp = Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol

withUdp :: (Socket.Socket -> IO a) -> IO a
withUdp = bracket createUdp Socket.close

createTcp :: IO Socket.Socket
createTcp = Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol

withTcp :: (Socket.Socket -> IO a) -> IO a
withTcp = bracket createTcp Socket.close

orFail :: (MonadFail m) => Maybe a -> String -> m a
orFail value message = case value of
  Just x -> return x
  Nothing -> fail message

thenFail :: (MonadFail m) => Maybe a -> (a -> String) -> m ()
thenFail value createMessage = case value of
  Just x -> fail $ createMessage x
  Nothing -> return ()

convert :: (Integral a, Integral b) => a -> b
convert = fromInteger . toInteger

timeout :: Int -> IO a -> IO a
timeout mcs action = do
  (var :: MVar (Either SomeException a)) <- newEmptyMVar

  producer <- forkIO $ flip catch (\(e :: SomeException) -> putMVar var $ Left e) $ do
    result <- action
    putMVar var $ Right result

  killer <- forkIO $ do
    threadDelay mcs
    putMVar var $ Left $ SomeException TimeoutException

  exceptionOrRes <- takeMVar var
  mapM_ killThread [producer, killer]
  case exceptionOrRes of
    Right res -> return res
    Left exception -> throwIO exception