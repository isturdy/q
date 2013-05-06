module Main where

import           Test.Framework           (Test, defaultMain)

import           Data.Queue.RealtimeTests
import           Data.Queue.SeqTests
import           Data.Queue.SimpleTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    simpleTests
  , realtimeTests
  , seqTests
  ]
