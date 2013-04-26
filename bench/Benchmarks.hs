module Main where

import Criterion.Main
import Criterion.Config

import Data.Queue.SimpleBench
import Data.Queue.RealtimeBench

main = defaultMainWith myConfig (return ())  [
    simpleBench
  , realtimeBench
  ]

myConfig = defaultConfig {
              -- Always GC between runs.
              cfgPerformGC = ljust True
            }
