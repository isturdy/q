{-# LANGUAGE Safe #-}

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
    SimpleQueue
  , Queue (..)

  -- ** Utility
  , force

  ) where

import           Control.Monad
import           Data.List
import           Data.Monoid
import           Text.Read

import           Data.Queue.Class

data SimpleQueue a = SimpleQueue [a] [a]
-- Invariant: the first list is empty only if the second is also empty

instance Functor SimpleQueue where
  fmap g (SimpleQueue f b) = SimpleQueue (fmap g f) (fmap g b)

-- | Appending queues of length /m/ and /n/ is worst-case/O/(/m/+/n/)
instance Monoid (SimpleQueue a) where
  mempty = empty
  mappend (SimpleQueue f1 b1) (SimpleQueue f2 b2) =
    SimpleQueue (f1++reverse b1++f2) b2

instance Read a => Read (SimpleQueue a) where
  readPrec = parens . prec 10 $ do
    Ident "fromList" <- lexP
    liftM fromList readPrec

  readListPrec = readListPrecDefault

instance Show a => Show (SimpleQueue a) where
  showsPrec d q  = showParen (d > 10) $
    showString "fromList " . shows (toList q)

instance Eq a => Eq (SimpleQueue a) where
  q1 == q2 = toList q1 == toList q2

instance Ord a => Ord (SimpleQueue a) where
  compare q1 q2 = compare (toList q1) (toList q2)

instance Queue SimpleQueue where
  empty = SimpleQueue [] []

  -- | /O/(1) worst case.
  fromList l = SimpleQueue l []

  -- | /O/(1) worst case.
  enq (SimpleQueue [] []) e = SimpleQueue [e] []
  enq (SimpleQueue f  b ) e = SimpleQueue f (e:b)

  -- | /O/(1) worst case.
  addList = foldl' enq

  -- | /O/(1) amortized, /O/(/n/) worst case.
  deq (SimpleQueue []    []) = SimpleQueue [] []
  deq (SimpleQueue [_]   b ) = SimpleQueue (reverse b) []
  deq (SimpleQueue (_:f) b ) = SimpleQueue f b
  deq _                      = SimpleQueue [] []

  -- | /O/(1) worst case.
  peek (SimpleQueue (h:_) _) = Just h
  peek _                     = Nothing

  -- | /O/(1).
  isEmpty (SimpleQueue [] []) = True
  isEmpty _                   = False

  -- | /O/(/n/).
  toList (SimpleQueue f b) = f++reverse b

  foldq g acc (SimpleQueue f b) = foldr (flip g) (foldl g acc f) b

-- | /O/(/n/). Force processing of a queue; until elements are
-- added, all operations with /O/(1) amortized time will have /O/(1)
-- worst case. Note that this is less efficient than waiting for the
-- queue to trigger the processing on its own.
force :: SimpleQueue a -> SimpleQueue a
force q = SimpleQueue (toList q) []
