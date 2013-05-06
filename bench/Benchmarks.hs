module Main where

import           Criterion.Config
import           Criterion.Main

import           Data.Queue.RealtimeBench
import           Data.Queue.SimpleBench

main = defaultMainWith myConfig (return ())  [
    simpleBench
  , realtimeBench
  ]

myConfig = defaultConfig {
              -- Always GC between runs.
              cfgPerformGC = ljust True
            }
