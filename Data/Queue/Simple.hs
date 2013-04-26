{-# LANGUAGE Safe          #-}
{-# LANGUAGE TupleSections #-}

---------------------------------------------------------------------------
-- |
-- Module      :  Data.Queue.Simple
-- Maintainer  :  sturdyi12@mail.wlu.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions to 'Text.Read')
--
-- An efficient purely functional FIFO queue. Based on the algorithm from
--
--   * Okasaki, Chris. 1996. /Purely Functional Data Structures/.
--     Thesis, Carnegie Mellon University. p 18.
--     <https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf>
--
-- The functions 'deq' and 'pop' have a worst case of /O/(/n/), but run
-- in amortized constant time: interspersing /n/ additions and /n/ removals
-- will take total time proportional to /n/, so the average cost of removal
-- is constant. This guarantee is broken if the queue is not emptied (the
-- first removal after /n/ insertions to an empty queue takes time
-- /O/(/n/)) or if multiple removals are made from the same iteration of
-- the queue.
---------------------------------------------------------------------------

module Data.Queue.Simple (
  -- * Queue type
    Queue

  -- ** Construction
  , fromList

  -- ** Insertion
  , enq
  , addList

  -- ** Access/Removal
  , peek
  , deq
  , pop

  -- ** Recursion
  , foldq
  , qrec
  , qrecM

  -- ** Utility
  , isEmpty
  , toList
  , force

  ) where

import           Control.Monad
import           Data.List
import           Data.Monoid
import           Text.Read

data Queue a = Queue [a] [a]
-- Invariant: the first list is empty only if the second is also empty

instance Functor Queue where
  fmap g (Queue f b) = Queue (fmap g f) (fmap g b)

-- | Appending queues of length /m/ and /n/ is worst-case/O/(/m/+/n/)
instance Monoid (Queue a) where
  mempty = Queue [] []
  mappend (Queue f1 b1) (Queue f2 b2) = Queue (f1++reverse b1++f2) b2

instance Read a => Read (Queue a) where
  readPrec = parens . prec 10 $ do
    Ident "fromList" <- lexP
    liftM fromList readPrec

  readListPrec = readListPrecDefault

instance Show a => Show (Queue a) where
  showsPrec d q  = showParen (d > 10) $
    showString "fromList " . shows (toList q)

instance Eq a => Eq (Queue a) where
  q1 == q2 = toList q1 == toList q2

instance Ord a => Ord (Queue a) where
  compare q1 q2 = compare (toList q1) (toList q2)

-- | /O/(/n/). Force processing of a queue; until elements are
-- added, all operations with /O/(1) amortized time will have /O/(1)
-- worst case.
force :: Queue a -> Queue a
force q = Queue (toList q) []

-- | /O/(1) worst case. Create a queue from a list.
fromList :: [a] -> Queue a
fromList l = Queue l []

-- | /O/(/n/). Turn a queue into a list in order of removal.
toList :: Queue a -> [a]
toList (Queue f b) = f++reverse b

-- | /O/(1) worst case. Add an element to the back of the queue.
enq :: Queue a -> a -> Queue a
enq (Queue [] []) e = Queue [e] []
enq (Queue f b) e = Queue f (e:b)

-- | /O(m)/ worst case. Add a list (of length /m/) to the queue.
addList :: Queue a -> [a] -> Queue a
addList = foldl' enq

-- | /O/(1) worst case. Look at the first element of the queue. Returns
-- 'Nothing' iff the queue is empty.
peek :: Queue a -> Maybe a
peek (Queue (h:_) _) = Just h
peek _               = Nothing

-- | /O/(1) amortized, /O/(/n/) worst case. Drop the first element of the
--  queue; if the queue is empty, return it unchanged.
deq :: Queue a -> Queue a
deq (Queue []    []) = Queue [] []
deq (Queue [_]   b ) = Queue (reverse b) []
deq (Queue (_:f) b ) = Queue f b
deq _                = Queue [] []

-- | /O/(1) amortized, /O/(/n/) worst case. Return 'Just' the first element
-- and the tail of the queue, or 'Nothing' for an empty queue.
pop :: Queue a -> Maybe (a, Queue a)
pop q = liftM (,deq q) $ peek q

-- | /O/(1) worst case. Determine if a queue is empty.
isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

-- Recursive combinators
-- | Fold over a queue in removal order.
foldq :: (a -> b -> a) -> a -> Queue b -> a
foldq g acc (Queue f b) = foldr (flip g) (foldl g acc f) b

-- | General recursion over queues. The function is from an accumulator
-- and item from the queue to a new accumulator and list of items to add
-- to the back of the queue.
qrec :: (a -> b -> (a,[b])) -> a -> Queue b -> a
qrec f a q = maybe a process (pop q)
  where process (x,q') = let (a',l) = f a x
                         in  qrec f a' (addList q' l)

-- | A monadic version of 'qrec'
qrecM :: Monad m => (a -> b -> m (a,[b])) -> a -> Queue b -> m a
qrecM f a q = maybe (return a) process (pop q)
  where process (x,q') = do (a',l) <- f a x
                            qrecM f a' (addList q' l)
