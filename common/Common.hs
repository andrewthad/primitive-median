module Common
  ( naiveIntegralMedian
  ) where

naiveIntegralMedian :: (Ord a, Num a) => [a] -> a
naiveIntegralMedian xs
  | sz < 1 = 0
  | odd sz = sortedList !! (div sz 2)
  | otherwise = div ((sortedList !! (div sz 2)) + (sortedList !! (div sz 2 + 1))) 2
  where
  sz = L.length xs
  m = foldr (\x m -> M.insertWith (+) x 1) M.empty xs
  sortedList = M.foldrWithKey (\x ct xs -> replicate ct x ++ xs) [] m

