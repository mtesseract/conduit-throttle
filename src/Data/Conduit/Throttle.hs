{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Data.Conduit.Throttle where

import           Conduit
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import           Control.Concurrent.Throttle     (ThrottleConf)
import qualified Control.Concurrent.Throttle     as Throttle
import           Control.Monad.Trans.Resource
import           UnliftIO

throttleProducer :: (MonadUnliftIO m, MonadResource m)
                 => ThrottleConf Int
                 -> Producer m Int
                 -> Producer m Int
throttleProducer throttleConf producer = do
  (UnliftIO unlifter) <- lift askUnliftIO
  queueIn  <- liftIO $ newTBMQueueIO 1024
  queueOut <- liftIO $ newTBMQueueIO 1
  (_, _) <- allocate
    (UnliftIO.async (unlifter (runConduit (producer .| drainConduit queueIn))))
    UnliftIO.cancel

  let
      readCallback  = atomically (readTBMQueue queueIn)
      writeCallback = \case
        Just a  -> atomically $ writeTBMQueue queueOut a
        Nothing -> atomically $ closeTBMQueue queueOut

  (_, asyncThrottler) <- allocate
    (Throttle.throttle throttleConf readCallback writeCallback)
    UnliftIO.cancel
  link asyncThrottler
  go queueOut

  where go queue = do
          liftIO (atomically (readTBMQueue queue)) >>= \case
            Just a  -> yield a >> go queue
            Nothing -> return ()

        drainConduit queue = do
          await >>= \case
            Just a  -> do liftIO (atomically (writeTBMQueue queue a))
                          drainConduit queue
            Nothing -> liftIO (atomically (closeTBMQueue queue))
