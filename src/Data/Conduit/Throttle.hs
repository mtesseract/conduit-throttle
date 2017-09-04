{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Conduit.Throttle
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
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TBMQueue
import qualified Control.Concurrent.Throttle     as Throttle
import           Control.Monad.Trans.Resource
import           Data.Function
import           UnliftIO

data Conf a = Conf
  { _measure       :: Throttle.Measure a
  , _interval      :: Double
  , _maxThroughput :: Double
  , _bufferSize    :: Int
  , _emaAlpha      :: Double
  }

newConf :: Conf a
newConf = defaultConf

-- | Default 'ThrottleConf'.
defaultConf :: Conf a
defaultConf = Conf
  { _measure       = const 1
  , _interval      = 1000
  , _maxThroughput = 100
  , _bufferSize    = 1024
  , _emaAlpha      = defaultEmaAlpha }

-- | Set measure function in configuration.
setMeasure :: Throttle.Measure a
           -> Conf a
           -> Conf a
setMeasure measure conf = conf { _measure = measure }

-- | Set interval in configuration.
setInterval :: Double
            -> Conf a
            -> Conf a
setInterval interval conf = conf { _interval = interval }

-- | Set max throughput in configuration.
setMaxThroughput :: Double
                 -> Conf a
                 -> Conf a
setMaxThroughput throughput conf =
  conf { _maxThroughput = throughput }

-- | Set buffer size in configuration.
setBufferSize :: Int
              -> Conf a
              -> Conf a
setBufferSize n conf = conf { _bufferSize = n }

-- | Set exponential weight factor used for computing current item
-- size.
setEmaAlpha :: Double
            -> Conf a
            -> Conf a
setEmaAlpha alpha conf = conf { _emaAlpha = alpha }

-- | Default exponential weight factor for computing current item
-- size.
defaultEmaAlpha :: Double
defaultEmaAlpha = 0.5

throttleConfPrepare :: Conf a -> Throttle.ThrottleConf a
throttleConfPrepare Conf { .. } = Throttle.newThrottleConf
  & Throttle.throttleConfThrottleProducer
  & Throttle.throttleConfSetMeasure _measure
  & Throttle.throttleConfSetInterval _interval
  & Throttle.throttleConfSetMaxThroughput _maxThroughput
  & Throttle.throttleConfSetBufferSize _bufferSize
  & Throttle.throttleConfSetEmaAlpha _emaAlpha

-- | Given a 'ThrottleConf' and a 'Producer', create and return a new
-- 'Producer', which yields the same stream of values like the
-- provided producer but throttled according to the provided
-- throttling configuration.
throttleProducer :: (MonadUnliftIO m, MonadResource m)
                 => Conf a
                 -> Producer m a
                 -> Producer m a
throttleProducer conf producer = do
  (UnliftIO unlifter) <- lift askUnliftIO
  queueIn  <- liftIO $ newTBMQueueIO 1024
  queueOut <- liftIO $ newTBMQueueIO 1
  (_, _) <- allocate
    (UnliftIO.async (unlifter (runConduit (producer .| drainConduit queueIn))))
    UnliftIO.cancel

  let throttleConf  = throttleConfPrepare conf
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
