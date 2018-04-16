{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -O2 -Wall #-}

module Data.Primitive.PrimArray.Median
  ( int8
  , int
  , word8
  , word
  ) where

import Data.Word
import Data.Primitive.PrimArray
import Control.Monad.Primitive

-- This is not exported. It's difficult to trust GHC to always specialize
-- exported functions. Since there are a limited number of types for
-- which we are interested in using this function, we simply do all of
-- the specialization in this module.
medianTemplate :: (Prim a, Ord a) => MutablePrimArray s a -> ST s a
medianTemplate !m = error "write me"

specInt8 :: MutablePrimArray s a -> ST s a
specInt8 x = medianTemplate x

int8 :: MutablePrimArray (PrimState m) Int8 -> m Int8
int8 = unsafePrimToST . specInt8

specWord8 :: MutablePrimArray s a -> ST s a
specWord8 x = medianTemplate x

word8 :: MutablePrimArray (PrimState m) Int8 -> m Int8
word8 = unsafePrimToST . specInt8
