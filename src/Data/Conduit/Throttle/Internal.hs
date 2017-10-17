{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Conduit.Throttle.Internal
  ( Conf
  , newConf
  , setMeasure
  , setInterval
  , setMaxThroughput
  , setBufferSize
  , setEmaAlpha
  , throttleConfPrepare
  ) where

import qualified Control.Concurrent.Throttle as Throttle
import           Data.Function

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

