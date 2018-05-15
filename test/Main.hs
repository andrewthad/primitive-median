{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import GHC.Exts (fromList)
import Data.Ord
import qualified Data.Map.Strict as M
import qualified GHC.OldList as L

import qualified Data.Primitive.PrimArray.Median as PAM
import Control.Monad.ST
import Data.Primitive.PrimArray
import Data.Primitive.Types (Prim)

import Data.Int (Int8)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "MEDIAN"
  [ QC.testProperty "Int8"
      $ \xs -> naiveIntegralMedian xs === fastMedian PAM.int8 xs
  , QC.testProperty "Word8"
      $ \xs -> naiveIntegralMedian xs === fastMedian PAM.word8 xs
  ]

fastMedian :: (Prim a) => (forall s. MutablePrimArray s a -> ST s a) -> [a] -> a
fastMedian f xs = runST $ do
  let arr = fromList xs
  marr <- newPrimArray (sizeofPrimArray arr)
  copyPrimArray marr 0 arr 0 (sizeofPrimArray arr)
  f marr

naiveIntegralMedian :: (Integral a, Ord a) => [a] -> a
naiveIntegralMedian xs
  | sz < 1 = 0
  | otherwise = sortedList !! (div sz 2)
  where
    sz = L.length xs
    sortedList = L.sort xs
