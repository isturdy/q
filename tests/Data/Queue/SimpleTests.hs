module Data.Queue.SimpleTests (simpleTests) where

import           Data.Maybe
import           Data.Monoid
import           Test.Framework                       (testGroup)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Property

import           Data.Queue.Simple

-- The challenge here is to ensure that all functions work identically
-- regardless of the internal state of the queue.

simpleTests = testGroup "Data.Queue.Simple" [
    consistencyTests
  ]

consistencyTests = testGroup "Consistency" [
    testCase     "empty"      $ isEmpty mempty @?= True
  , testProperty "isEmpty"      prop_empty
  , testProperty "to/from list" prop_id_list
  , testProperty "mappend"      prop_mappend
  , testProperty "peek"         prop_id_peek
  , testProperty "Read/Show"    prop_id_show
  , testProperty "force"        prop_id_force
  , testProperty "foldq"        prop_id_foldq
  , testProperty "addList"      prop_id_addList
  , testCase     "qrec"       $ qrec (count_if_less 10) 0 (fromList [1]) @?= 143
  ]

prop_empty l1 l2 = (l1++l2==[]) == isEmpty q
  where q = toQueue l1 l2 :: Queue Int

prop_id_list :: [Int] -> [Int] -> Bool
prop_id_list l1 l2 = (l1++l2) == toList (toQueue l1 l2)

prop_mappend l11 l12 l21 l22 = fromList (l11++l12++l21++l22) == q1 <> q2
  where q1 = toQueue l11 l12 :: Queue Int
        q2 = toQueue l21 l22 :: Queue Int

prop_id_peek l1 l2 = case (l1++l2) of
  []    -> peek q == Nothing
  (h:_) -> peek q == Just h
  where q = toQueue l1 l2 :: Queue Int

prop_id_show l1 l2 = q == (read $ show q)
  where q = toQueue l1 l2 :: Queue Int

prop_id_force l1 l2 = q == force q
  where q = toQueue l1 l2 :: Queue Int

prop_id_foldq l1 l2 = foldl (+) 0 (l1++l2) == foldq (+) 0 q
  where q = toQueue l1 l2 :: Queue Int

prop_id_addList l1 l2 l3 = fromList (l1++l2++l3) == addList q l3
  where q = toQueue l1 l2 :: Queue Int

-- Makes a queue from two lists; equivalent to '\l1 l2 -> fromList (l1++l2)'
toQueue :: [a] -> [a] -> Queue a
toQueue [] l  = fromList l
toQueue l1 l2 = fromList l1 `addList` l2

count_if_less :: Int -> Int -> Int -> (Int, [Int])
count_if_less n a e | e > n = (a,[])
count_if_less n a e         = (a+1,[e+1,e+2])
