{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Conduit
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Throttle
import qualified Control.Concurrent.Throttle    as Throttle
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Conduit.List              (sourceList)
import           Data.Conduit.Throttle
import           Data.Function                  ((&))
import           Data.Typeable
import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     ((@?=))

main :: IO ()
main = do
  putStrLn ""
  defaultMain tests

tests :: [Test.Framework.Test]
tests =
  [ testGroup "Test Suite" [ testCase "Simple producer throttling" simpleProducerThrottling ] ]

simpleProducerThrottling :: IO ()
simpleProducerThrottling = do
  let conf = newThrottleConf
             & throttleConfThrottleProducer
             & throttleConfSetInterval 1000
             & throttleConfSetMaxThroughput 1
  runResourceT . runConduit $ throttleProducer conf (sourceList [1..5]) .| printC
