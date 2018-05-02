module Common
  ( naiveIntegralMedian
  ) where

import qualified Data.List as L
import qualified Data.Map as M

naiveIntegralMedian :: (Integral a, Ord a) => [a] -> a
naiveIntegralMedian xs
  | sz < 1 = 0
  | odd sz = sortedList !! (div sz 2)
  | otherwise = div ((sortedList !! (div sz 2)) + (sortedList !! (div sz 2 - 1))) 2
  where
  sz = L.length xs
  sortedList = L.sort xs
