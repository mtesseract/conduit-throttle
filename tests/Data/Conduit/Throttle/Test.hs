{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Conduit
import           Data.Conduit.Async
import           Data.Conduit.List              (sourceList)
import qualified Data.Conduit.Throttle          as ConduitThrottle
import           Data.Function                  ((&))
import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = do
  putStrLn ""
  defaultMain tests

tests :: [Test.Framework.Test]
tests =
  [ testGroup "Test Suite"
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
