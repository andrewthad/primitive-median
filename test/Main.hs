{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import GHC.Exts (fromList)
import Data.Ord
import qualified Data.Map.Strict as M
import qualified GHC.OldList as L

import qualified Data.Primitive.PrimArray.Foo as PAM
import Control.Monad.ST
import Data.Primitive.PrimArray
import Data.Primitive.Types (Prim)

import Data.Int (Int8)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Median"
  [ QC.testProperty "int8"
      $ \xs -> naiveIntegralMedian xs === fastMedian PAM.int8 xs
  , QC.testProperty "word8"
      $ \xs -> naiveIntegralMedian xs === fastMedian PAM.word8 xs
  ]

int8 :: [Int8] -> Int8
int8 = fastMedian PAM.int8

fastMedian :: (Prim a) => (forall s. MutablePrimArray s a -> ST s a) -> [a] -> a
fastMedian f xs = runST $ do
  let arr = fromList xs
  marr <- newPrimArray (sizeofPrimArray arr)
  copyPrimArray marr 0 arr 0 (sizeofPrimArray arr)
  f marr

naiveIntegralMedian :: (Integral a, Ord a) => [a] -> a
naiveIntegralMedian xs
  | sz < 1 = 0
  | odd sz = sortedList !! (div sz 2)
  | otherwise = div ((sortedList !! (div sz 2)) + (sortedList !! (div sz 2 - 1))) 2
  where
  sz = L.length xs
  sortedList = L.sort xs
