import Test.Tasty
import Test.Tasty.QuickCheck as QC

import GHC.Exts (fromList)
import Data.Ord
import qualified Data.Map.Strict as M
import qualified GHC.OldList as L

import Common (naiveIntegralMedian)

tests :: TestTree
tests = testGroup "Median"
  [ QC.testProperty "int8"
      $ \xs -> naiveMedian xs === fastMedian PAM.int8
  , QC.testProperty "word8"
      $ \xs -> naiveMedian xs === fastMedian PAM.word8
  ]

fastMedian :: (forall s. MutablePrimArray s a -> ST s a) -> [a] -> a
fastMedian f xs = runST $ do
  let arr = fromList xs
  marr <- newPrimArray (sizeofPrimArray arr)
  copyPrimArray marr 0 arr 0 (sizeofPrimArray arr)
  f marr

