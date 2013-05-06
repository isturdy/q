{-# LANGUAGE Safe #-}

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
-- 'pop') all have constant worst-case time, at the cost of greater
-- overhead than the implimentation in 'Data.Queue.Simple'.
--
-- This algorithm differs slightly from Okasaki's, in that it does not
-- memoize the results of forcing lazy values. However, since the key
-- operations have /O/(1) worst-case complexity, it still offers real-time
-- performance.
---------------------------------------------------------------------------

module Data.Queue.Realtime (
  -- * Queue type
    RealtimeQueue
  , Queue (..)

  ) where

import           Control.Monad
import           Data.List
import           Data.Monoid
import           Text.Read

import           Data.Queue.Class

data RealtimeQueue a = RealtimeQueue [a] [a] [a]
-- Invariant: length of the third list equals the difference in lengths
-- of the other two.

-- Incremental reverse + append (internal).
rotate :: [a] -> [a] -> [a] -> [a]
rotate []     (y:[]) a = y:a
rotate (x:xs) (y:ys) a = x : rotate xs ys (y:a)
rotate _ _ _ = error "Bug in library Data.Queue.Realtime: An invariant \
                     \varied. Please notify the library maintainer."

-- Smart constructor (internal). Enforces invariant
queue :: [a] -> [a] -> [a] -> RealtimeQueue a
queue [] [] []  = RealtimeQueue [] [] []
queue f b (_:t) = RealtimeQueue f b t
queue f b []    = RealtimeQueue f' [] f'
  where f' = rotate f b []

instance Functor RealtimeQueue where
  fmap g (RealtimeQueue f b s) = RealtimeQueue (fmap g f) (fmap g b) (fmap g s)

-- | Appending queues of length /m/ and /n/ is worst-case/O/(/m/+/n/)
instance Monoid (RealtimeQueue a) where
  mempty = empty
  mappend = foldq enq

instance Read a => Read (RealtimeQueue a) where
  readPrec = parens . prec 10 $ do
    Ident "fromList" <- lexP
    liftM fromList readPrec

  readListPrec = readListPrecDefault

instance Show a => Show (RealtimeQueue a) where
  showsPrec d q  = showParen (d > 10) $
    showString "fromList " . shows (toList q)

instance Eq a => Eq (RealtimeQueue a) where
  q1 == q2 = toList q1 == toList q2

instance Ord a => Ord (RealtimeQueue a) where
  compare q1 q2 = compare (toList q1) (toList q2)

instance Queue RealtimeQueue where
  empty = RealtimeQueue [] [] []

  -- | /O/(/n/).
  fromList l = RealtimeQueue l [] l

  -- | /O/(1).
  enq (RealtimeQueue f b s) e = queue f (e:b) s

  -- | /O/(1).
  deq (RealtimeQueue (_:f) b s) = queue f b s
  deq _                         = RealtimeQueue [] [] []

  -- | /O/(1).
  peek (RealtimeQueue (h:_) _ _) = Just h
  peek _                         = Nothing

  -- | /O(m)/.
  addList = foldl' enq

  foldq g acc (RealtimeQueue f b _) = foldr (flip g) (foldl g acc f) b

  -- | /O/(/n/).
  toList (RealtimeQueue f b _) = f++reverse b

  -- | /O/(1).
  isEmpty (RealtimeQueue [] [] _) = True
  isEmpty _             = False
