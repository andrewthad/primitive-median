{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -O2 -Wall #-}

{-|
The algorithm used in all of these variants is quickselect. It takes
the median of an array in average case /O(n)/ time, where /n/ is the length
of the array. A description of this algorithm can be found
on wikipedia: https://en.wikipedia.org/wiki/Quickselect.
It destroys the mutable array it is given as an argument and provides
the caller its median element. Consequently, taking the median of an immutable array
requires first allocating a mutable array and filling it with the
immutable array's contents using 'copyPrimArray'.

The variant of quickselect provided here does not use random pivots.
This library uses pregenerated random number used as the source
of entropy. The effect on the argument array, while destructive, is entirely
deterministic. Because the pivot chosen in a deterministic fashion,
an adversarial user could choose an input that degrades the performance
of the algorithm to /O(n^2)/. Do not use this on input provided by
a user expected to be adversarial.

Mathematically, median is not defined for empty arrays. However, instead
of throwing an error, all median variants provided in this module will
return 0 when given an empty array as input. If the length of the array
divides two evenly, there is no middle element. This library follows
the common convention of taking the arithmetic mean of the two middle
elements in this case.
-}
module Data.Primitive.PrimArray.Median
  ( int8
  , word8
  ) where

import Control.Monad.Primitive
import Control.Monad.ST (runST,ST)
import Data.Int
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Word

-- This is not exported. It's difficult to trust GHC to always specialize
-- exported functions. Since there are a limited number of types for
-- which we are interested in using this function, we simply do all of
-- the specialization in this module. Notice that we specialize the monad
-- to ST and then generalize it later in type-specific functions. This
-- makes it possible to avoid needing specialized copy of each type-specific
-- function for both IO and for ST.
medianTemplate :: (Prim a, Ord a) => MutablePrimArray s a -> ST s a
medianTemplate !m = error "write me"

indexEntropy :: Int -> Int
indexEntropy !ix = indexPrimArray entropy ((entropyCount - 1) .&. ix)

entropyCount :: Int
entropyCount = 16

entropy :: PrimArray Int
entropy = runST $ do
  m <- newPrimArray entropyCount
  writePrimArray m  0 (0x64C7D840A9DD9517 :: Int)
  writePrimArray m  1 (0x8C222C5F3E120F73 :: Int)
  writePrimArray m  2 (0xDDE6A2B788A962BA :: Int)
  writePrimArray m  3 (0x318AA78133F5C815 :: Int)
  writePrimArray m  4 (0x94DE99B9C5035ADA :: Int)
  writePrimArray m  5 (0x304935BCE8C92867 :: Int)
  writePrimArray m  6 (0x34F584518B5333BB :: Int)
  writePrimArray m  7 (0xAB4BAD95AE9F11FB :: Int)
  writePrimArray m  8 (0x3C612675F491B05E :: Int)
  writePrimArray m  9 (0xA842BAF0C6A39FAE :: Int)
  writePrimArray m 10 (0xF4669E626DCDEAB9 :: Int)
  writePrimArray m 11 (0x36B064B35405BA10 :: Int)
  writePrimArray m 12 (0x5AA8FF325AD1CF66 :: Int)
  writePrimArray m 13 (0x5385F92445FFD7CD :: Int)
  writePrimArray m 14 (0x8F1900754853CA6A :: Int)
  writePrimArray m 15 (0xB7CB335C587B727C :: Int)
  unsafeFreezePrimArray m

specInt8 :: MutablePrimArray s Int8 -> ST s Int8
specInt8 x = medianTemplate x

int8 :: PrimMonad m => MutablePrimArray (PrimState m) Int8 -> m Int8
int8 = stToPrim . specInt8

specWord8 :: MutablePrimArray s Word8 -> ST s Word8
specWord8 x = medianTemplate x

word8 :: PrimMonad m => MutablePrimArray (PrimState m) Int8 -> m Int8
word8 = stToPrim . specInt8
