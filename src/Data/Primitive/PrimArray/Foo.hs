{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

--{-# OPTIONS_GHC -O2 -Wall -fno-warn-name-shadowing #-}

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
|-}

module Data.Primitive.PrimArray.Foo where

import Control.Monad.Primitive
import Control.Monad.ST (runST,ST)
import Data.Bits
import Data.Int
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Word

-- insertion sort
sort
  :: forall m a. (Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> m ()
sort !mpa
  | l == 0 = pure ()
  | l == 1 = pure ()
  | l == 2 = do
      !a0 <- readPrimArray mpa 0
      !a1 <- readPrimArray mpa 1
      if a0 < a1
        then pure ()
        else do
          !_ <- writePrimArray mpa 0 a1
          !_ <- writePrimArray mpa 0 a0
          pure ()
  | otherwise = do
      let go !ix = if ix < l
            then do
              let go' !jx = do
                    !ajm1 <- readPrimArray mpa (jx - 1)
                    !aj   <- readPrimArray mpa jx
                    if (jx > 0 && ajm1 > aj)
                      then do
                        !_ <- swap mpa jx (jx - 1)
                        go' (jx - 1)
                      else pure ()
              go' ix
            else pure ()
      go 1
  where
    !l = sizeofMutablePrimArray mpa

median
  :: forall m a. (Num a, Ord a, Prim a, PrimMonad m)
  => (a -> a) -- ^ div by 2
  -> MutablePrimArray (PrimState m) a
  -> m a
median f !mpa
  | l < 1 = pure 0
--  | l < 6 = do
--      !_ <- sort mpa
--      if even l
--        then do
--          !t0 <- readPrimArray mpa (div l 2)
--          !t1 <- readPrimArray mpa (div l 2 + 1)
--          pure $! f (t0 + t1)
--        else do
--          !t0 <- readPrimArray mpa (div l 2)
--          pure $! t0
  | even l = do
      !t0 <- select mpa 0 0 (l - 1) (div l 2 - 1)
      !t1 <- select mpa 0 0 (l - 1) (div l 2)
      pure $! f (t0 + t1)
  | otherwise = do
      !t0 <- select mpa 0 0 (l - 1) (div l 2)
      pure $! t0
  where
    !l = sizeofMutablePrimArray mpa

partition
  :: forall m a. (Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ left-most index
  -> Int -- ^ right-most index
  -> Int -- ^ pivot index
  -> m Int
partition !mpa !left !right !pivotIndex = do
  !pivotValue <- readPrimArray mpa pivotIndex
  !_ <- swap mpa pivotIndex right
  let go !i !storeIndex = if i < right
        then do
          !listAtI <- readPrimArray mpa i
          if listAtI < pivotValue
            then do
              !_ <- swap mpa storeIndex i 
              go (i + 1) (storeIndex + 1)
            else pure ()
        else pure ()
  !_ <- go left left
  !_ <- swap mpa right (right - 1)
  pure (right - 1)

select
  :: forall m a. (Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ iteration 
  -> Int -- ^ left-most index
  -> Int -- ^ right-most index
  -> Int -- ^ k
  -> m a -- ^ k-th smallest element within [left .. right]
select !mpa !iter !left !right !k
  | left == right = do -- list contains only one element
      !el <- readPrimArray mpa left
      pure $! el
  | otherwise = do
      let !p = left + (indexEntropy iter `mod` remainingStuffLen)
          !remainingStuffLen = right - left + 1
      !pivotIndex <- partition mpa left right p
      if k == pivotIndex
        then do
          !listAtK <- readPrimArray mpa k
          pure $! listAtK
        else if k < pivotIndex
          then select mpa (iter + 1) left (pivotIndex - 1) k
          else select mpa (iter + 1) (pivotIndex + 1) right k

swap
  :: forall m a. (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int
  -> Int
  -> m ()
swap !mpa !a !b = do
  a' <- readPrimArray mpa a
  b' <- readPrimArray mpa b
  writePrimArray mpa a b'
  writePrimArray mpa b a'

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

medianFractional
  :: forall m a. (Fractional a, Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> m a
medianFractional = median (\x -> x / 2)

medianIntegral
  :: forall m a. (Integral a, Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> m a
medianIntegral = median (\x -> div x 2)

int8 :: PrimMonad m => MutablePrimArray (PrimState m) Int8 -> m Int8
int8 x = stToPrim $! medianIntegral x

word8 :: PrimMonad m => MutablePrimArray (PrimState m) Word8 -> m Word8
word8 x = stToPrim $! medianIntegral x

