{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Conduit
import           Data.Conduit.Async
import           Data.Conduit.List              (sourceList)
import qualified Data.Conduit.Throttle          as ConduitThrottle
import           Data.Function                  ((&))
import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)

import qualified Data.Conduit.Throttle.MBC.Test
import qualified Data.Conduit.Throttle.Test

main :: IO ()
main = do
  putStrLn ""
  defaultMain tests

tests :: [Test.Framework.Test]
tests =
  Data.Conduit.Throttle.Test.tests
  ++ Data.Conduit.Throttle.MBC.Test.tests
