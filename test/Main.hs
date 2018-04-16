import Test.Tasty
import Test.Tasty.QuickCheck as QC

import GHC.Exts (fromList)
import Data.Ord
import qualified Data.Map.Strict as M
import qualified GHC.OldList as L

tests :: TestTree
tests = testGroup "Median"
  [ QC.testProperty "int8"
      $ \xs -> naiveMedian xs === fastMedian PAM.int8
  , QC.testProperty "word8"
      $ \xs -> naiveMedian xs === fastMedian PAM.word8
  ]

naiveMedian :: (Ord a, Num a) => [a] -> a
naiveMedian xs
  | sz < 1 = 0
  | odd sz = sortedList !! (div sz 2)
  | otherwise = div ((sortedList !! (div sz 2)) + (sortedList !! (div sz 2 + 1))) 2
  where
  sz = L.length xs
  m = foldr (\x m -> M.insertWith (+) x 1) M.empty xs
  sortedList = M.foldrWithKey (\x ct xs -> replicate ct x ++ xs) [] m

fastMedian :: (forall s. MutablePrimArray s a -> ST s a) -> [a] -> a
fastMedian f xs = runST $ do
  let arr = fromList xs
  marr <- newPrimArray (sizeofPrimArray arr)
  copyPrimArray marr 0 arr 0 (sizeofPrimArray arr)
  f marr

