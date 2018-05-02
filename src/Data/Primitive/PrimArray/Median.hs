{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

--{-# OPTIONS_GHC -O2 -Wall #-}

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

nth
  :: forall a m. (Num a, Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int --     |
  -> m a --     v 
         -- 0,1,2,3,4,5,6,7
         -- l = 8
         -- ix = 2
         -- length of ys = 3
         -- length of zs = l - length of ys = 5
         -- offset into mpa of z = lengthOfYs
         -- 
nth !mpa !n = do 
  !ix <- partition' mpa
  if (ix + 1) == n
    then readPrimArray mpa 0
    else if (ix + 1) > n
      then do
        !ys <- resizeMutablePrimArray mpa (ix + 1)
        nth ys n
      else do
        !zs <- newPrimArray (l - ix)
        copyMutablePrimArray zs 0 mpa (ix + 1) (l - (ix + 1))
        nth zs $! n - (ix + 1) - 1
  where
    !l = sizeofMutablePrimArray mpa

kth
  :: forall a m. (Num a, Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int
  -> Int
  -> Int
  -> m a
kth !mpa !l !r !k = if (k > 0 && k <= r - l + 1)
  then do
    !ix <- partition mpa l r
    if (ix - l == k - 1)
      then readPrimArray mpa ix
      else if (ix - l > k - 1)
        then kth mpa l (ix - 1) k
        else kth mpa (ix + 1) r (k - ix + l - 1)
  else pure 0

-- This is not exported. It's difficult to trust GHC to always specialize
-- exported functions. Since there are a limited number of types for
-- which we are interested in using this function, we simply do all of
-- the specialization in this module. Notice that we specialize the monad
-- to ST and then generalize it later in type-specific functions. This
-- makes it possible to avoid needing specialized copy of each type-specific
-- function for both IO and for ST.
medianTemplate
  :: forall a m. (Num a, Ord a, Prim a, PrimMonad m)
  => (a -> a) -- divide by 2
  -> MutablePrimArray (PrimState m) a
  -> m a
medianTemplate !f !mpa
  | n < 1  = pure 0
  | even n = do
      !t0 <- nth mpa (div n 2)
      !t1 <- nth mpa (div n 2 - 1)
      pure $! f (t0 + t1)
  | otherwise = nth mpa (div n 2)
  where
    !n = sizeofMutablePrimArray mpa

partition' :: (PrimMonad m, Prim a, Ord a) => MutablePrimArray (PrimState m) a -> m Int
partition' !mpa = do
  let !left = 0
      !right = sizeofMutablePrimArray mpa
  partition mpa left right

partition :: (PrimMonad m, Prim a, Ord a) => MutablePrimArray (PrimState m) a -> Int -> Int -> m Int
partition !mpa !left !right = do
  let !i = left
      !j = left
  !x <- readPrimArray mpa right
  !atStart <- readPrimArray mpa j
  !ix <- go x j (right - 1) i mpa
  swap mpa ix right
  pure i

go :: (PrimMonad m, Prim a, Ord a)
  => a
  -> Int -- starting point, j
  -> Int -- stopping point, r - 1
  -> Int -- return value, i
  -> MutablePrimArray (PrimState m) a
  -> m Int
go !x !l !r !ix !mpa = if l <= r
  then do
    !isLessThanEq <- lessThanEq mpa l x
    if isLessThanEq
      then do
        swap mpa ix l
        go x (l + 1) r (ix + 1) mpa
      else
        pure ix
  else pure ix

-- is the given value of type 'a' less than or equal to the value at the index?
lessThanEq
  :: (PrimMonad m, Prim a, Ord a)
  => MutablePrimArray (PrimState m) a
  -> Int -- index
  -> a   -- element
  -> m Bool
lessThanEq !mpa !ix !el = do
  !x <- readPrimArray mpa ix
  if x <= el
    then pure True
    else pure False

foldlMutablePrimArray'
  :: forall m a b. (Prim a, PrimMonad m)
  => (b -> a -> b)
  -> b
  -> MutablePrimArray (PrimState m) a
  -> m b
foldlMutablePrimArray' f z0 mpa = go 0 z0
  where
    !sz = sizeofMutablePrimArray mpa
    go !i !acc = if i < sz
      then do
        ix <- readPrimArray mpa i
        go (i + 1) (f acc ix)
      else pure acc

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

integralDivBy2 :: Integral a => a -> a
integralDivBy2 x = div x 2

fractionalDivBy2 :: Fractional a => a -> a
fractionalDivBy2 x = x / 2

specInt8 :: MutablePrimArray s Int8 -> ST s Int8
specInt8 x = medianTemplate integralDivBy2 x

int8 :: PrimMonad m => MutablePrimArray (PrimState m) Int8 -> m Int8
int8 = stToPrim . specInt8

specWord8 :: MutablePrimArray s Word8 -> ST s Word8
specWord8 x = medianTemplate integralDivBy2 x

word8 :: PrimMonad m => MutablePrimArray (PrimState m) Word8 -> m Word8
word8 = stToPrim . specWord8
