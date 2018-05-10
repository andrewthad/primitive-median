{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Primitive.PrimArray.Poo where

import Control.Monad.Primitive
import Data.Int
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Data.Word

type Comparison a = a -> a -> Ordering

sort3ByIndex
  :: forall m a. (Prim a, PrimMonad m)
  => Comparison a
  -> MutablePrimArray (PrimState m) a
  -> Int
  -> Int
  -> Int
  -> m ()
sort3ByIndex cmp !mpa !i !j !k = do
  a0 <- readPrimArray mpa i
  a1 <- readPrimArray mpa j
  a2 <- readPrimArray mpa k

  case cmp a0 a1 of
    GT -> case cmp a0 a2 of
            GT -> case cmp a2 a1 of
                    LT -> do writePrimArray mpa i a2
                             writePrimArray mpa k a0
                    _  -> do writePrimArray mpa i a1
                             writePrimArray mpa j a2
                             writePrimArray mpa k a0
            _  -> do writePrimArray mpa i a1
                     writePrimArray mpa j a0
    _  -> case cmp a1 a2 of
            GT -> case cmp a0 a2 of
                    GT -> do writePrimArray mpa i a2
                             writePrimArray mpa j a0
                             writePrimArray mpa k a1
                    _  -> do writePrimArray mpa j a2
                             writePrimArray mpa k a1
            _  -> return ()

medianTemplate
  :: forall m a. (Num a, Prim a, PrimMonad m)
  => Comparison a
  -> MutablePrimArray (PrimState m) a
  -> Int -- ^ lower index, l
  -> Int -- ^ upper index, u
  -> m a
medianTemplate cmp !mpa !l !u
  | len < 1 = pure 0
  | even len = pure 0
  | otherwise = do
      let !c = div (u + l) 2 
      !_ <- sort3ByIndex cmp mpa c l (u - 1)
      !meddy <- readPrimArray mpa 0 -- l
      pure meddy
  where
    !len = sizeofMutablePrimArray mpa

medianIntegral
  :: forall m a. (Ord a, Num a, Prim a, PrimMonad m)
  => MutablePrimArray (PrimState m) a
  -> m a
medianIntegral !mpa = medianTemplate compare mpa 0 l
  where
    !l = sizeofMutablePrimArray mpa

int8
  :: forall m. PrimMonad m
  => MutablePrimArray (PrimState m) Int8
  -> m Int8
int8 = stToPrim . medianIntegral

word8
  :: forall m. PrimMonad m
  => MutablePrimArray (PrimState m) Word8
  -> m Word8
word8 = stToPrim . medianIntegral
