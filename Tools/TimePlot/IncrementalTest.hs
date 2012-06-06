module Tools.TimePlot.IncrementalTest where

import Test.QuickCheck hiding(collect)
import Test.HUnit
import Data.Map as M
import Tools.TimePlot.Incremental

finalization_of_collecting_a_list_is_the_list xs =  
  runStreamSummary collect  xs == xs

stateful_test = TestList [
  runStreamSummary (stateful 0 (+) id) ([1..10] :: [Int]) ~?= sum [1 .. 10]  
  ]
                
collect_by_time_bins_test = TestList [
  runStreamSummary (byTimeBins [1,5,10] collect) [ (i,i) | i <- [1 .. 9]] ~?= [(1,[1,2,3,4]),(5,[5,6,7,8,9])]  
  ]
                 
collect_by_keys_test = TestList [
  runStreamSummary (byKey $ const collect) [ (i `mod` 3,i) | i <- [1 .. 9]] ~?= M.fromList [(0,[3,6,9]),(1,[1,4,7]),(2,[2,5,8])]
  ]
                 
summary_tests = test [
  stateful_test,
  collect_by_time_bins_test,
  collect_by_keys_test
  ]
