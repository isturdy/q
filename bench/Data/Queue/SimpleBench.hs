module Data.Queue.SimpleBench (simpleBench) where

import           Criterion.Main
import           Data.Monoid

import           Data.Queue.Simple

simpleBench = bgroup "simple" [
    bench "sequential 10k" $ whnf enq_pop l10k
  , bench "sequential 50k" $ whnf enq_pop l50k
  , bench "fromList 50k"   $ whnf (foldq const 0 . fromList) l50k
  , bench "addList 50k"    $ whnf (flip addList l50k) q100k
  , bench "foldq 100k"     $ whnf (foldq const 0) q100k
  , bench "qrec 20"        $ whnf (qrec (add_if_less 20) 0) (fromList [1])
  ]


l10k :: [Int]
l10k = [1..10000]

l50k :: [Int]
l50k = [1..50000]

q100k :: Queue Int
q100k = fromList l50k `addList` l50k

-- This gives a benchmark with a good mix of additions and removals.
-- Computational complexity is incredible--be careful exceeding 30.
add_if_less :: Int -> Int -> Int -> (Int, [Int])
add_if_less n a e | e > n = (e,[])
add_if_less n a e         = (e,[e+1,e+2])

-- Enqueue a bunch of things and then pop them all. Only triggers one reverse.
enq_pop :: [Int] -> ()
enq_pop = pop_all . foldl enq mempty

pop_all :: Queue a -> ()
pop_all q = go (pop q)
  where go (Just (e,q')) = go $ pop q'
        go Nothing       = ()
