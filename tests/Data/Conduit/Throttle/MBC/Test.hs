{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.Throttle.MBC.Test (tests) where

import           Conduit
import           Data.Conduit.Async
import           Data.Conduit.List              (sourceList)
import qualified Data.Conduit.Throttle.MBC      as ConduitThrottle
import           Data.Function                  ((&))
import           Test.Framework                 (Test, testGroup)
import           Test.Framework.Providers.HUnit (testCase)

tests :: [Test.Framework.Test]
tests =
  [ testGroup "Test Suite [MonadBaseControl version]"
    [ testCase "Simple producer throttling"
      simpleProducerThrottling
    , testCase "Simple producer throttling (using stm-conduit)"
      simpleProducerThrottlingStm
    ]
  ]

simpleProducerThrottling :: IO ()
simpleProducerThrottling = do
  let conf = ConduitThrottle.newConf
             & ConduitThrottle.setInterval 1000
             & ConduitThrottle.setMaxThroughput 1
  runResourceT . runConduit $
    ConduitThrottle.throttleProducer conf (sourceList [1..5]) .| printC

simpleProducerThrottlingStm :: IO ()
simpleProducerThrottlingStm = do
  let conf = ConduitThrottle.newConf
             & ConduitThrottle.setInterval 1000
             & ConduitThrottle.setMaxThroughput 1
  runResourceT . runCConduit $
    ConduitThrottle.throttleProducer conf (sourceList [1..5]) =$=& printC
