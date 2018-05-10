{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Primitive.PrimArray.Foo where

import Control.Monad.Primitive
import Control.Monad.ST (runST)
import Data.Bits
import Data.Int
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Word

import Prelude hiding (mapM_)

median
  :: forall m a. (Num a, Prim a, PrimMonad m)
  => (a -> a -> a)
  -> MutablePrimArray (PrimState m) a
  -> m a
median div2 !mpa
  | len < 1 = pure 0
  | odd len = pure $! findMid (id) 0 (numToCount - (0))
  | otherwise = pure $! findMid2 div2 (id) 0 (numToCount - (0))
  where
    numToCount = div len 2 + 1
    !len = sizeofMutablePrimArray mpa

zeroes
  :: forall m a. (Num a, Prim a, PrimMonad m)
  => Int
  -> m (MutablePrimArray (PrimState m) a)
zeroes !sz = do
  !new <- newPrimArray sz
  !_ <- setPrimArray new 0 sz 0
  pure new

makeCounts
  :: forall m a. (Num a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> m (MutablePrimArray (PrimState m) a)
makeCounts !mpa = do
  !counts <- zeroes sz
  forM_ mpa $! (constPure $! modifyMutablePrimArray counts ((+1) . fromIntegral))
  pure counts
  where
    !sz = sizeofMutablePrimArray mpa

constPure :: Applicative m => b -> a -> m a
constPure _ a = pure a

findMid :: Num a => (Int -> Int) -> Int -> Int -> a
findMid f !i !numRemaining
  | numRemaining <= 0 = fromIntegral i
  | otherwise = findMid f (i + 1) (numRemaining - f (i + 1))

findMid2 :: Num a => (a -> a -> a) -> (Int -> Int) -> Int -> Int -> a
findMid2 avg f !i !numRemaining =
  case compare numRemaining 0 of
    LT -> fromIntegral i
    GT -> findMid2 avg f (i + 1) (numRemaining - f (i + 1))
    EQ -> midAverage (i + 1) (f (i + 1))
      where
        midAverage j 0 = midAverage (j + 1) (f (j + 1))
        midAverage j _ = avg (fromIntegral i) (fromIntegral j)

-- | Modify everything in the MutablePrimArray.
modifyMutablePrimArray
  :: forall m a. (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> (a -> a)
  -> m ()
modifyMutablePrimArray !mpa f = do
  let go !ix = if ix < sz
        then do
          !a <- readPrimArray mpa ix
          !_ <- writePrimArray mpa ix (f a)
          go (ix + 1)
        else pure ()
  go 0
  where
    !sz = sizeofMutablePrimArray mpa

mapM_
  :: forall m a. (Prim a, PrimMonad m)
  => (a -> m a)
  -> MutablePrimArray (PrimState m) a 
  -> m ()
mapM_ f !mpa = do
  let go !ix = if ix < sz
        then do
          !a <- readPrimArray mpa ix
          !b <- f a
          !_ <- writePrimArray mpa ix b
          go (ix + 1)
        else pure ()
  go 0
  where
    !sz = sizeofMutablePrimArray mpa

forM_
  :: forall m a. (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> (a -> m a)
  -> m ()
forM_ = flip mapM_
