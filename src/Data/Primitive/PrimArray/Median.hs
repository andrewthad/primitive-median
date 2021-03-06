{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Primitive.PrimArray.Median
  ( int
  , int8
  , int16
  , int32
  , int64
  
  , word
  , word8
  , word16
  , word32
  , word64

  , median
  ) where

import Control.Monad (when)
import Control.Monad.Primitive
import Control.Monad.ST (runST,ST)
import Data.Bits
import Data.Int
import Data.Foldable (foldlM)
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Word
import Debug.Trace

median
  :: forall m a. (Num a, Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> m a
median !mpa
  | l < 1 = pure 0
  | otherwise = do
      !_ <- quickSort mpa
      !melody <- readPrimArray mpa (div l 2)
      -- !melody <- quickSelect mpa (div l 2)
      return melody
  where
    !l = sizeofMutablePrimArray mpa

foreachWith
  :: forall t m a. (Foldable t, Monad m)
  => t a
  -> a
  -> (a -> a -> m a)
  -> m a
foreachWith xs v f = foldlM (flip f) v xs

-- | Partition takes an index,
-- and sorts the mutable prim array into three
-- sections: the elements less than the element
-- at that index, the element of the index itself,
-- and the elements greater than the index.
-- 
-- It returns the final index of the element.
partition
  :: forall m a. (Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ pivot index
  -> m Int
partition !mpa !k = partitionInternal mpa 0 (l - 1) k
  where
    !l = sizeofMutablePrimArray mpa

partitionInternal
  :: forall m a. (Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ left-most index
  -> Int -- ^ right-most index
  -> Int -- ^ pivot index
  -> m Int
partitionInternal !mpa !left !right !pivotIndex = do
  !pivotValue <- readPrimArray mpa pivotIndex
  !_ <- swap mpa pivotIndex right
  storeIndex <- foreachWith [left .. right - 1] left (\i storeIndex -> do
    val <- readPrimArray mpa i
    if (val <= pivotValue)
      then do
        !_ <- swap mpa i storeIndex
        return (storeIndex + 1)
      else return storeIndex )
  !_ <- swap mpa storeIndex right
  return storeIndex

quickSort
  :: forall m a. (Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> m ()
quickSort !mpa = quickSortInternal mpa 0 0 (l - 1)
  where
    !l = sizeofMutablePrimArray mpa

quickSortInternal
  :: forall m a. (Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int -- iteration 
  -> Int -- left
  -> Int -- right
  -> m ()
quickSortInternal !mpa !iter !left !right = when (right > left) $ do
  let !remainingStuffLen = right - left + 1
      !pivotIndex = left + (indexEntropy iter `mod` remainingStuffLen)
  !newPivot <- partitionInternal mpa left right pivotIndex
  quickSortInternal mpa (iter + 1) left (newPivot - 1)
  quickSortInternal mpa (iter + 1) (newPivot + 1) right

quickSelect
  :: forall m a. (Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ index of k-th smallest element within [0 .. length - 1]
  -> m a
quickSelect !mpa !k = quickSelectInternal mpa 0 0 (l - 1) k
  where
    !l = sizeofMutablePrimArray mpa

quickSelectInternal
  :: forall m a. (Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int -- ^ iteration 
  -> Int -- ^ left-most index
  -> Int -- ^ right-most index
  -> Int -- ^ k
  -> m a -- ^ k-th smallest element within [left .. right]
quickSelectInternal !mpa !iter !left !right !k
  | left == right = do -- list contains only one element
      !el <- readPrimArray mpa left
      pure $! el
  | otherwise = do
      let !remainingStuffLen = right - left + 1
          !pivotIndex = left + (indexEntropy iter `mod` remainingStuffLen)
      !newPivot <- partitionInternal mpa left right pivotIndex
      let -- (S1, S2, S3) are the parts of the mutable primitive array
          -- that are less than, equal to, and greater than the pivot,
          -- respectively.
          !sz = sizeofMutablePrimArray mpa 
          !szS1 = newPivot
          !szS2 = 1
          !szS3 = sz - newPivot- 1
      if szS1 >= k
        then quickSelectInternal mpa (iter + 1) left (newPivot - 1) k
        else if szS1 + szS2 >= k
          then do
            !listAtPivot <- readPrimArray mpa newPivot
            pure $! listAtPivot
          else quickSelectInternal mpa (iter + 1) (newPivot + 1) right (k - szS1 - szS2)

swap
  :: forall m a. (Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> Int
  -> Int
  -> m ()
swap !mpa !a !b = do
  !a' <- readPrimArray mpa a
  !b' <- readPrimArray mpa b
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

int :: PrimMonad m => MutablePrimArray (PrimState m) Int -> m Int
int x = stToPrim $! median x

int8 :: PrimMonad m => MutablePrimArray (PrimState m) Int8 -> m Int8
int8 x = stToPrim $! median x

int16 :: PrimMonad m => MutablePrimArray (PrimState m) Int16 -> m Int16
int16 x = stToPrim $! median x

int32 :: PrimMonad m => MutablePrimArray (PrimState m) Int32 -> m Int32
int32 x = stToPrim $! median x

int64 :: PrimMonad m => MutablePrimArray (PrimState m) Int64 -> m Int64
int64 x = stToPrim $! median x

word :: PrimMonad m => MutablePrimArray (PrimState m) Word -> m Word
word x = stToPrim $! median x

word8 :: PrimMonad m => MutablePrimArray (PrimState m) Word8 -> m Word8
word8 x = stToPrim $! median x

word16 :: PrimMonad m => MutablePrimArray (PrimState m) Word16 -> m Word16
word16 x = stToPrim $! median x

word32 :: PrimMonad m => MutablePrimArray (PrimState m) Word32 -> m Word32
word32 x = stToPrim $! median x

word64 :: PrimMonad m => MutablePrimArray (PrimState m) Word64 -> m Word64
word64 x = stToPrim $! median x

