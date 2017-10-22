{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Framework                 (Test, defaultMain)

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
