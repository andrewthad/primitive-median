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

module Data.Primitive.PrimArray.Median
  ( int8
  , word8
  ) where

import Control.Monad.Primitive
import Control.Monad.ST (runST,ST)
import Data.Bits
import Data.Int
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Word

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

partition
  :: forall m a. (Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a -- ^ stuff
  -> Int      -- ^ left-most index
  -> Int      -- ^ right-most index
  -> Int      -- ^ pivot index
  -> m Int    -- ^ pivot
partition !mpa !left !right !pivotIndex = do
  pivotValue <- readPrimArray mpa pivotIndex
  _ <- swap mpa pivotIndex right -- move pivot to end 
  storeIndex <- go mpa 0 left left right pivotValue
  _ <- swap mpa right storeIndex -- move pivot to its final place 
  pure storeIndex
  where
    go
      :: forall m a. (Prim a, Ord a, PrimMonad m)
      => MutablePrimArray (PrimState m) a
      -> Int
      -> Int
      -> Int
      -> Int
      -> a
      -> m Int
    go !mpa !storeIx !ix !left !right !pivotValue =
      if (ix >= left && ix < right)
        then do
          atIx <- readPrimArray mpa ix    
          if atIx < pivotValue
            then do
              _ <- swap mpa storeIx ix
              go mpa (storeIx + 1) (ix + 1) left right pivotValue
            else pure storeIx
        else pure storeIx

debugMode :: Bool
debugMode = True

partialSort
  :: forall m a. (Ord a, Prim a, PrimMonad m)
  => Int -- ^ iteration
  -> MutablePrimArray (PrimState m) a -- ^ stuff
  -> Int -- ^ slice left
  -> Int -- ^ slice right
  -> Int -- ^ final left
  -> Int -- ^ final right
  -> m ()
partialSort !iter !mpa !sl !sr !fl !fr
  | (sl == fl && sr == fr) = pure ()
  | debugMode && fl < sl = debugInfo "left invariant violated"
  | debugMode && fr > sr = debugInfo "right invariant violated"
  | debugMode && sr < sl = debugInfo "invariant violated"
  | debugMode && iter > 50000 = debugInfo "ran too long"
  | otherwise = do
    let !remainingStuffLen = sr - sl + 1
        !p = sl + (indexEntropy iter `mod` remainingStuffLen)
    !pivotIndex <- partition mpa sl sr p
    if | pivotIndex == sl -> if pivotIndex == fl
           then partialSort (iter + 1) mpa pivotIndex sr fr fr
           else partialSort (iter + 1) mpa (sl + 1) sr fl fr
       | pivotIndex == sr -> if pivotIndex == fr
           then partialSort (iter + 1) mpa sl pivotIndex fl fl
           else partialSort (iter + 1) mpa sl (sr - 1) fl fr
       | fl < pivotIndex -> if True -- what was here
           then partialSort (iter + 1) mpa pivotIndex sr fl fr
           else partialSort (iter + 1) mpa sl pivotIndex fl fr
  where
  debugInfo descr = error
    ("partialSort: " ++ descr ++ " [size=" ++ show (sizeofMutablePrimArray mpa) ++
     ",sl=" ++ show sl ++ ",sr=" ++ show sr ++ ",fl=" ++ show fl ++
     ",fr=" ++ show fr ++ ",iter=" ++ show iter ++ "]"
    )

medianTemplate
  :: forall m a. (Num a, Ord a, Prim a, PrimMonad m)
  => (a -> a) -- divide by 2
  -> MutablePrimArray (PrimState m) a
  -> m a
medianTemplate !f !mpa
  | l < 1 = pure 0
  | even l = do
      let !fl = div l 2 - 1
          !fr = div l 2
          !sl = 0
          !sr = l - 1
      !_ <- partialSort 0 mpa sl sr fl fr
      !t0 <- readPrimArray mpa fl
      !t1 <- readPrimArray mpa fr
      pure $! f (t0 + t1)
  | otherwise = do
      let !fl = div l 2 
          !fr = div l 2
          !sl = 0
          !sr = l - 1
      !_ <- partialSort 0 mpa sl sr fl fr
      !t0 <- readPrimArray mpa fr
      pure t0
  where
    !l = sizeofMutablePrimArray mpa

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
medianFractional = medianTemplate (\x -> x / 2)

medianIntegral
  :: forall m a. (Integral a, Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> m a
medianIntegral = medianTemplate (\x -> div x 2)

int8 :: PrimMonad m => MutablePrimArray (PrimState m) Int8 -> m Int8
int8 x = stToPrim $! medianIntegral x

word8 :: PrimMonad m => MutablePrimArray (PrimState m) Word8 -> m Word8
word8 x = stToPrim $! medianIntegral x

