{-# LANGUAGE Safe          #-}
{-# LANGUAGE TupleSections #-}

---------------------------------------------------------------------------
-- |
-- Module      :  Data.Queue.Simple
-- Maintainer  :  sturdyi12@mail.wlu.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions to 'Text.Read')
--
-- An purely functional FIFO queue with key operations in
-- constant worst-case time. Based on the algorithm from
--
--   * Okasaki, Chris. 1996. /Purely Functional Data Structures/.
--     Thesis, Carnegie Mellon University. p 43.
--     <https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf>
--
-- The key operations 'enq', 'peek', and 'deq' (along with the derivative
-- 'pop') all run in constant time, at the cost of greater overhead than
-- the implimentation in 'Data.Queue.Simple'.
---------------------------------------------------------------------------

module Data.Queue.Realtime (
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

  ) where

import           Control.Monad
import           Data.List
import           Data.Monoid
import           Text.Read

data Queue a = Queue [a] [a] [a]
-- Invariant: length of the third list equals the difference in lengths
-- of the other two.

-- Incremental reverse + append (internal).
rotate :: [a] -> [a] -> [a] -> [a]
rotate []     (y:[]) a = y:a
rotate (x:xs) (y:ys) a = x : rotate xs ys (y:a)
rotate _ _ _ = error "Bug in library Data.Queue.Realtime: An invariant \
                     \varied. Please notify the library maintainer."

-- Smart constructor (internal). Enforces invariant
queue :: [a] -> [a] -> [a] -> Queue a
queue [] [] []  = Queue [] [] []
queue f b (_:t) = Queue f b t
queue f b []    = Queue f' [] f'
  where f' = rotate f b []

instance Functor Queue where
  fmap g (Queue f b s) = Queue (fmap g f) (fmap g b) (fmap g s)

-- | Appending queues of length /m/ and /n/ is worst-case/O/(/m/+/n/)
instance Monoid (Queue a) where
  mempty = Queue [] [] []
  mappend = foldq enq

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

-- | /O/(/n/). Create a queue from a list.
fromList :: [a] -> Queue a
fromList l = Queue l [] l

-- | /O/(/n/). Turn a queue into a list in order of removal.
toList :: Queue a -> [a]
toList (Queue f b _) = f++reverse b

-- | /O/(1). Add an element to the back of the queue.
enq :: Queue a -> a -> Queue a
enq (Queue f b s) e = queue f (e:b) s

-- | /O(m)/. Add a list (of length /m/) to the queue.
addList :: Queue a -> [a] -> Queue a
addList = foldl' enq

-- | /O/(1). Look at the first element of the queue. Returns
-- 'Nothing' iff the queue is empty.
peek :: Queue a -> Maybe a
peek (Queue (h:_) _ _) = Just h
peek _                 = Nothing

-- | /O/(1). Drop the first element of the
--  queue; if the queue is empty, return it unchanged.
deq :: Queue a -> Queue a
deq (Queue (_:f) b s) = queue f b s
deq _                 = Queue [] [] []

-- | /O/(1). Return 'Just' the first element
-- and the tail of the queue, or 'Nothing' for an empty queue.
pop :: Queue a -> Maybe (a, Queue a)
pop q = liftM (,deq q) $ peek q

-- | /O/(1). Determine if a queue is empty.
isEmpty :: Queue a -> Bool
isEmpty (Queue [] [] _) = True
isEmpty _             = False

-- Recursive combinators
-- | Fold over a queue in removal order.
foldq :: (a -> b -> a) -> a -> Queue b -> a
foldq g acc (Queue f b _) = foldr (flip g) (foldl g acc f) b

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
