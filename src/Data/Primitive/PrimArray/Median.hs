{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import Control.Monad (mapM_)
import Control.Monad.Primitive
import Control.Monad.ST (runST,ST)
import Data.Bits
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
medianTemplate
  :: forall s a. (Num a, Ord a, Prim a)
  => MutablePrimArray s a
  -> ST s a
medianTemplate !m = if l == 0
  then pure 0
  else select 0 m 0 l (div l 2)
  where
    l = sizeofMutablePrimArray m

partition
  :: forall s a. (Ord a, Prim a)
  => MutablePrimArray s a -- ^ stuff
  -> Int      -- ^ left-most index
  -> Int      -- ^ right-most index
  -> Int      -- ^ pivot index
  -> ST s Int -- ^ pivot
partition !mpa !left !right !pivotIndex = do
  pivotValue <- readPrimArray mpa pivotIndex
  _ <- swap mpa pivotIndex right -- move pivot to end 
  storeIndexM :: MutablePrimArray s Int <- newPrimArray 1
  _ <- writePrimArray storeIndexM 0 left 
  _ <- mapM_ (\i -> do
      atI <- readPrimArray mpa i
      if atI < pivotValue
        then do
          storeIndex <- readPrimArray storeIndexM 0
          swap mpa storeIndex i 
          _ <- incrementSingleton storeIndexM
          pure () 
        else pure ()) [left .. right - 1]
  storeIndex <- readPrimArray storeIndexM 0 
  _ <- swap mpa right storeIndex -- move pivot to its final place 
  pure storeIndex
 
incrementSingleton :: (Prim a, PrimMonad m, Num a) => MutablePrimArray (PrimState m) a -> m ()
incrementSingleton !mpa = do
  i <- readPrimArray mpa 0
  writePrimArray mpa 0 (i + 1)

swap
  :: (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int
  -> Int
  -> m ()
swap !mpa !a !b = do
  a' <- readPrimArray mpa a
  b' <- readPrimArray mpa b
  writePrimArray mpa a b'
  writePrimArray mpa b a'

select
  :: forall s a. (Ord a, Prim a)
  => Int                    -- ^ iteration
  -> MutablePrimArray s a   -- ^ stuff
  -> Int                    -- ^ left-most index
  -> Int                    -- ^ right-most index
  -> Int                    -- ^ k
  -> ST s a
select !iter !mpa !left !right !k = do
  pivotIndex <- partition mpa left right p 
  if k == right
    then readPrimArray mpa left
    else if k == pivotIndex
      then readPrimArray mpa k
      else if k < pivotIndex
        then do
          select (iter + 1) mpa left (pivotIndex - 1) k
        else do
          select (iter + 1) mpa (pivotIndex + 1) right k
  where
    p = indexEntropy iter `mod` (l - remainingStuffLen) + left
    remainingStuffLen = left - right + 1
    l = sizeofMutablePrimArray mpa

indexEntropy :: Int -> Int
indexEntropy !ix = indexPrimArray entropy ((entropyCount - 1) .&. ix)

entropyCount :: Int
entropyCount = 16

wordToInt :: Word -> Int
wordToInt = fromIntegral

entropy :: PrimArray Int
entropy = runST $ do
  m <- newPrimArray entropyCount
  writePrimArray m  0 (wordToInt (0x64C7D840A9DD9517 :: Word))
  writePrimArray m  1 (wordToInt (0x8C222C5F3E120F73 :: Word))
  writePrimArray m  2 (wordToInt (0xDDE6A2B788A962BA :: Word))
  writePrimArray m  3 (wordToInt (0x318AA78133F5C815 :: Word))
  writePrimArray m  4 (wordToInt (0x94DE99B9C5035ADA :: Word))
  writePrimArray m  5 (wordToInt (0x304935BCE8C92867 :: Word))
  writePrimArray m  6 (wordToInt (0x34F584518B5333BB :: Word))
  writePrimArray m  7 (wordToInt (0xAB4BAD95AE9F11FB :: Word))
  writePrimArray m  8 (wordToInt (0x3C612675F491B05E :: Word))
  writePrimArray m  9 (wordToInt (0xA842BAF0C6A39FAE :: Word))
  writePrimArray m 10 (wordToInt (0xF4669E626DCDEAB9 :: Word))
  writePrimArray m 11 (wordToInt (0x36B064B35405BA10 :: Word))
  writePrimArray m 12 (wordToInt (0x5AA8FF325AD1CF66 :: Word))
  writePrimArray m 13 (wordToInt (0x5385F92445FFD7CD :: Word))
  writePrimArray m 14 (wordToInt (0x8F1900754853CA6A :: Word))
  writePrimArray m 15 (wordToInt (0xB7CB335C587B727C :: Word))
  unsafeFreezePrimArray m

specInt8 :: MutablePrimArray s Int8 -> ST s Int8
specInt8 x = medianTemplate x

int8 :: PrimMonad m => MutablePrimArray (PrimState m) Int8 -> m Int8
int8 = stToPrim . specInt8

specWord8 :: MutablePrimArray s Word8 -> ST s Word8
specWord8 x = medianTemplate x

word8 :: PrimMonad m => MutablePrimArray (PrimState m) Word8 -> m Word8
word8 = stToPrim . specWord8
