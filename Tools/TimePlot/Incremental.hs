{-# LANGUAGE GADTs, BangPatterns #-}
-- |A data structure for incrementally building some data from a stream of value
module Tools.TimePlot.Incremental where

import Data.Time
import qualified Data.List as L
import qualified Data.Map as M
import Control.Applicative
import Debug.Trace as D

data StreamSummary a r where
  Summary :: { insert :: a -> StreamSummary a r,  -- insert a value in stream producing a new summary
               finalize :: r                      -- extract result from stream
             } -> StreamSummary a r

instance Functor (StreamSummary a) where
  fmap f (Summary insert res) = Summary (fmap f . insert) (f res)

instance Applicative (StreamSummary a) where
  pure r = Summary (\_ -> pure r)  r
  (!fs) <*> (!xs) = Summary (\a -> insert fs a <*> insert xs a) (finalize fs $ finalize xs)

-- produce a result from running the stream over a list of values
runStreamSummary :: StreamSummary a r -> [a] -> r
runStreamSummary !s []     = finalize s
runStreamSummary !s (a:as) = runStreamSummary (insert s a) as

-- A stream that accumulates some value in a state s and combines 
-- newly inserted values with previous state, eg. a fold 
stateful :: s -> (a -> s -> s) -> (s -> r) -> StreamSummary a r
stateful init insert finalize = go init
  where
    go !s = Summary (\a -> go (insert a s)) (finalize s)

-- combines filter and map using over a stream of a to produce a stream of 
-- bs with elements removed
filterMap :: (a -> Maybe b) -> StreamSummary b r -> StreamSummary a r
filterMap p s@(Summary insert res) = Summary insert' res
  where 
    insert' a = case p a of { Nothing -> filterMap p s ; Just b -> filterMap p (insert b) }

-- map a stream of a to a stream of b
mapInput :: (a -> b) -> StreamSummary b r -> StreamSummary a r
mapInput f (Summary insert res) = Summary (mapInput f . insert . f) res

-- lift a list of values into a summary whose result is the list
collect :: StreamSummary a [a]
collect = stateful [] (:) reverse

-- group a stream of (time,value) couples in time "buckets" to produce a stream
-- of (time, [value]) elements
-- the list of bins and the list of timed values must be in increasing ordered
-- and the former must bound strictly all timestamps in the latter
byTimeBins :: (Ord t) => [t] -> StreamSummary (t,[a]) r -> StreamSummary (t,a) r
byTimeBins ts s = stateful init' insert' finalize'
  where
    init' = (ts, [], s)
    insert' (t,a) (t1:t2:ts, curBin, !s) 
      | t < t1 = error "Times are not in ascending order"
      | t < t2 = (t1:t2:ts, a:curBin, s)
      | True   = (t2:ts, [a], insert s (t1,reverse curBin))
    finalize' (t1:_:ts, curBin, s) = finalize (insert s (t1,reverse curBin))


-- build a stream of (key,value) pairs producing a Map result from a stream 
-- of values and a keying function
byKey :: (Ord k) => (k -> StreamSummary v r) -> StreamSummary (k,v) (M.Map k r)
byKey initByKey = stateful init' insert' finalize'
  where
    init' = M.empty
    insert' (k,v) m = case M.lookup k m of
      Nothing -> M.insert k (insert (initByKey k) v) m
      Just !s -> M.insert k (insert s v) m
    finalize' = fmap finalize
