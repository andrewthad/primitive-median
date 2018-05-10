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
import Data.Proxy (Proxy(..))
import Data.Word

spure :: Applicative f => a -> f a 
spure x = pure $! x
{-# INLINE spure #-}

lpure :: Applicative f => a -> f a
lpure = pure
{-# INLINE lpure #-}

medianIntegral
  :: forall m a. (Bounded a, Integral a, Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> m a
medianIntegral = constantSpaceMedian (fromIntegral) (div)

medianFloating
  :: forall m a. (Bounded a, RealFrac a, Ord a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> m a
medianFloating = constantSpaceMedian (floor) (/)

int8 :: PrimMonad m => MutablePrimArray (PrimState m) Int8 -> m Int8
int8 x = stToPrim $! medianIntegral x

word8 :: PrimMonad m => MutablePrimArray (PrimState m) Word8 -> m Word8
word8 x = stToPrim $! medianIntegral x

constantSpaceMedian
  :: forall m a. (Bounded a, Num a, Ord a, Prim a, PrimMonad m)
  => (a -> Int)
  -> (a -> a -> a) -- ^ divide by 2 for the even case
  -> MutablePrimArray (PrimState m) a
  -> m a -- ^ that there median...
constantSpaceMedian f divF !mpa
  | l < 1 = spure 0
  | odd l = do
      !count <- countOf f mpa 0
      findMid f mpa 0 (numToCount - (f count))  
  | otherwise = do
      !count <- countOf f mpa 0
      findMid2 divF f mpa 0 (numToCount - (f count))
  where
    !l = sizeofMutablePrimArray mpa
    !numToCount = div l 2 + 1

makesCountMutably
  :: forall m a. (Bounded a, Num a, Prim a, PrimMonad m)
  => (a -> Int)
  -> MutablePrimArray (PrimState m) a
  -> m (MutablePrimArray (PrimState m) a)
makesCountMutably !f !mpa = do
  let !n = numPossibleValues f (Proxy :: Proxy a) 
      !sz = sizeofMutablePrimArray mpa 
  !counts <- newPrimArray n -- do these start out at 0? probably not.
  let go !ix = if ix < sz
        then do
          -- !atIx <- readPrimArray counts ix
          !_ <- writePrimArray counts ix 1
          go (ix + 1)
        else pure ()
  go 0
  pure counts

numPossibleValues
  :: forall a. (Bounded a)
  => (a -> Int) -- ^ floor for Fractional types, fromIntegral for Integral types
  -> Proxy a    -- ^ the type
  -> Int        -- ^ number of possible values in the type
numPossibleValues f _ = f (maxBound :: a) + 1

-- make efficient table of counts for each possible value
countOf
  :: forall m a. (Bounded a, Num a, Prim a, PrimMonad m)
  => (a -> Int) 
  -> MutablePrimArray (PrimState m) a
  -> Int
  -> m a
countOf f !mpa !i = do
  !counts <- makesCountMutably f mpa
  !a <- readPrimArray counts i
  spure a

findMid
  :: forall m a. (Bounded a, Num a, Prim a, PrimMonad m)
  => (a -> Int)
  -> MutablePrimArray (PrimState m) a
  -> Int
  -> Int
  -> m a
findMid f !mpa !i !numRemaining
  | numRemaining < 1 = spure (fromIntegral i)
  | otherwise = do
      !count <- countOf f mpa (i + 1)
      findMid f mpa (i + 1) (numRemaining - f count)

findMid2
  :: forall m a. (Bounded a, Num a, Prim a, PrimMonad m)
  => (a -> a -> a)
  -> (a -> Int)
  -> MutablePrimArray (PrimState m) a
  -> Int
  -> Int
  -> m a
findMid2 divF f !mpa !i !numRemaining = 
  case compare numRemaining 0 of
    LT -> spure (fromIntegral i)
    GT -> do
      !count <- countOf f mpa (i + 1)
      findMid2 divF f mpa (i + 1) (numRemaining - f count)
    EQ -> do
      !count <- countOf f mpa (i + 1)
      midAverage divF f mpa i (i + 1) (f count) 

midAverage
  :: forall m a. (Bounded a, Num a, Prim a, PrimMonad m)
  => (a -> a -> a) -- div by 2 function
  -> (a -> Int)
  -> MutablePrimArray (PrimState m) a
  -> Int 
  -> Int
  -> Int
  -> m a
midAverage divF f !mpa !i !j 0 = do
  !count <- countOf f mpa (j + 1)
  midAverage divF f mpa i (j + 1) (f count)
midAverage divF _ _ !i !j _ = spure (divF (fromIntegral i) (fromIntegral j))

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

-- | Modify everything in the MutablePrimArray.
modifyMutablePrimArray
  :: (Prim a, PrimMonad m)
  => (a -> a)
  -> MutablePrimArray (PrimState m) a
  -> m ()
modifyMutablePrimArray f !mpa = do
  let go !ix = if ix < sz
        then do
          !a <- readPrimArray mpa ix
          !_ <- writePrimArray mpa ix (f a)
          go (ix + 1)
        else pure ()
  go 0
  where
    !sz = sizeofMutablePrimArray mpa


