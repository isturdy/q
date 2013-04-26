module Main where

import           Test.Framework                 (Test, defaultMain)

import           Data.Queue.SimpleTests
import           Data.Queue.RealtimeTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    simpleTests
  , realtimeTests
  ]
