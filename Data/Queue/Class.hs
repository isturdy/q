{-# LANGUAGE TupleSections #-}

---------------------------------------------------------------------------
-- |
-- Module      :  Data.Queue.Class
-- Maintainer  :  sturdyi12@mail.wlu.edu
-- Stability   :  provisional
-- Portability :  non-portable (TupleSections)
--
-- An abstract interface for FIFO queues.
---------------------------------------------------------------------------

module Data.Queue.Class where

import           Data.Foldable hiding (toList)
import qualified Data.Foldable as F
import qualified Data.Sequence as S

-- | A class for queues. Minimal definition is 'empty', 'enq', 'peek',
-- 'deq' (or 'empty', 'enq', and 'pop', but this will likely lead to an
-- inefficient 'peek'); others, 'isEmpty', 'foldq', 'fromList', and
-- 'toList' especially, may benefit from a specialized version. There
-- should be no need to overide the definitions of 'qrec' and 'qrecM'.
class Queue q where
  -- | Create an empty queue
  empty :: q a

  -- | Create a queue from a list.
  fromList :: [a] -> q a

  -- | Add an item to the back of a queue
  enq :: q a -> a -> q a

  -- | Add a list to the back of a queue.
  addList :: q a -> [a] -> q a

  -- | Inspect the first element of a queue.
  peek :: q a -> Maybe a
  peek = fmap fst . pop

  -- | Return a queue without its first element.
  deq :: q a -> q a
  deq q = maybe q snd $ pop q

  -- | Return 'Just' the first element
  -- and the tail of the queue, or 'Nothing' for an empty queue.
  pop :: q a -> Maybe (a, q a)
  pop q = fmap (,deq q) $ peek q

  -- | Convert a queue to a list in removal order. `toList . fromList == id`.
  toList :: q a -> [a]

  -- | Check whether a queue is empty
  isEmpty :: q a -> Bool
  isEmpty = maybe False (const True) . peek

  -- | Fold over a queue in removal order.
  foldq :: (a -> b -> a) -> a -> q b -> a

  -- | General recursion over queues. The function is from an accumulator
  -- and item from the queue to a new accumulator and list of items to add
  -- to the back of the queue.
  qrec :: Queue q => (a -> b -> (a,[b])) -> a -> q b -> a
  qrec f a q = maybe a process (pop q)
    where process (x,q') = let (a',l) = f a x
                           in  qrec f a' (addList q' l)

  -- | A monadic version of 'qrec'
  qrecM :: (Monad m, Queue q) => (a -> b -> m (a,[b])) -> a -> q b -> m a
  qrecM f a q = maybe (return a) process (pop q)
    where process (x,q') = do (a',l) <- f a x
                              qrecM f a' (addList q' l)

instance Queue S.Seq where
  empty = S.empty

  fromList = S.fromList

  enq = (S.|>)

  addList = foldl' enq

  deq q = case S.viewl q of
    S.EmptyL  -> q
    _ S.:< q' -> q'

  peek q = case S.viewl q of
    S.EmptyL -> Nothing
    e S.:< _ -> Just e

  foldq = foldl'

  toList = F.toList

  isEmpty = S.null
