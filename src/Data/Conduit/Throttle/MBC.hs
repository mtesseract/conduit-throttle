{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Data.Conduit.Throttle.MBC
  ( Conf
  , newConf
  , setMeasure
  , setInterval
  , setMaxThroughput
  , setBufferSize
  , setEmaAlpha
  , throttleProducer
  ) where

import           Conduit
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import qualified Control.Concurrent.Throttle     as Throttle
import           Control.Monad
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Data.Conduit.Throttle.Internal

-- | Given a 'ThrottleConf' and a 'Producer', create and return a new
-- 'Producer', which yields the same stream of values like the
-- provided producer but throttled according to the provided
-- throttling configuration.
throttleProducer :: (MonadIO m, MonadBaseControl IO m, MonadResource m)
                 => Conf o
                 -> ConduitM () o m ()
                 -> ConduitM () o m ()
throttleProducer conf producer = do
  queueIn  <- liftIO $ newTBMQueueIO 1024
  queueOut <- liftIO $ newTBMQueueIO 1
  conduitProcess <- lift $ liftBaseWith $ \runInBase ->
    return $ runInBase $ runConduit (producer .| drainConduit queueIn)
  void $ allocate (async conduitProcess) cancel

  let throttleConf  = throttleConfPrepare conf
      readCallback  = atomically (readTBMQueue queueIn)
      writeCallback = \case
        Just a  -> atomically $ writeTBMQueue queueOut a
        Nothing -> atomically $ closeTBMQueue queueOut

  (_, asyncThrottler) <- allocate
    (Throttle.throttle throttleConf readCallback writeCallback)
    cancel

  liftIO $ link asyncThrottler
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
