{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeInType                 #-}
#endif
{-# LANGUAGE UnboxedTuples              #-}

{-# OPTIONS_GHC
      -Weverything
      -fno-warn-unsafe
      -fno-warn-orphans
      -fno-warn-name-shadowing
      -fno-warn-missing-import-lists
      -fno-warn-implicit-prelude
      -O2
#-}

-- | Orphan instances for the 'Prim' typeclass.
module Data.Primitive.Instances () where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Complex
import Data.Functor.Const
#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity
#endif
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import Data.Ord
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive.Types
import GHC.Real
import Data.Word
import GHC.Fingerprint
import GHC.Prim
import GHC.Types

instance Prim a => Prim (Complex a) where
  sizeOf# _ = 2# *# sizeOf# (undefined :: a)
  alignment# _ = alignment# (undefined :: a)
  indexByteArray# arr# i# =
    let i = I# i#
        arr = ByteArray arr#
    in
         (indexByteArray arr (2 * i + 0))
      :+ (indexByteArray arr (2 * i + 1))
  readByteArray# :: forall s a. (Prim a) => MutableByteArray# s -> Int# -> State# s -> (# State# s, Complex a #)
  readByteArray# arr# i# = internal $ do
    let i = I# i#
        arr = MutableByteArray arr#
    a <- readByteArray arr (2 * i + 0) :: ST s a
    b <- readByteArray arr (2 * i + 1)
    return (a :+ b)
  writeByteArray# :: forall s a. (Prim a) => MutableByteArray# s -> Int# -> Complex a -> State# s -> State# s
  writeByteArray# arr# i# (a :+ b) = internal_ $ do
    let i = I# i#
        arr = MutableByteArray arr#
    writeByteArray arr (2 * i + 0) a
    writeByteArray arr (2 * i + 1) b :: ST s ()
  setByteArray# = defaultSetByteArray#
  indexOffAddr# :: Addr# -> Int# -> Complex a
  indexOffAddr# addr# i# =
    let i = I# i#
        addr = Addr addr#
    in (indexOffAddr addr (2 * i + 0)) :+ (indexOffAddr addr (2 * i + 1))
  readOffAddr# :: forall s a. (Prim a) => Addr# -> Int# -> State# s -> (# State# s, Complex a #)
  readOffAddr# addr# i# = internal $ do
    let i = I# i#
        addr = Addr addr#
    a <- readOffAddr addr (2 * i + 0) :: ST s a
    b <- readOffAddr addr (2 * i + 1)
    return (a :+ b)
  writeOffAddr# :: forall s a. (Prim a) => Addr# -> Int# -> Complex a -> State# s -> State# s
  writeOffAddr# addr# i# (a :+ b) = internal_ $ do
    let i = I# i#
        addr = Addr addr#
    writeOffAddr addr (2 * i + 0) a
    writeOffAddr addr (2 * i + 1) b :: ST s ()
  setOffAddr# = defaultSetOffAddr#

instance (Integral a, Prim a) => Prim (Ratio a) where
  sizeOf# _ = 2# *# sizeOf# (undefined :: a)
  alignment# _ = alignment# (undefined :: a)
  indexByteArray# arr# i# =
    let i = I# i#
        arr = ByteArray arr#
    in
         (indexByteArray arr (2 * i + 0))
      % (indexByteArray arr (2 * i + 1))
  readByteArray# :: forall s a. (Integral a, Prim a) => MutableByteArray# s -> Int# -> State# s -> (# State# s, Ratio a #)
  readByteArray# arr# i# = internal $ do
    let i = I# i#
        arr = MutableByteArray arr#
    a <- readByteArray arr (2 * i + 0) :: ST s a
    b <- readByteArray arr (2 * i + 1)
    return (a % b)
  writeByteArray# :: forall s a. (Prim a) => MutableByteArray# s -> Int# -> Ratio a -> State# s -> State# s
  writeByteArray# arr# i# (a :% b) = internal_ $ do
    let i = I# i#
        arr = MutableByteArray arr#
    writeByteArray arr (2 * i + 0) a
    writeByteArray arr (2 * i + 1) b :: ST s ()
  setByteArray# = defaultSetByteArray#
  indexOffAddr# :: Addr# -> Int# -> Ratio a
  indexOffAddr# addr# i# =
    let i = I# i#
        addr = Addr addr#
    in (indexOffAddr addr (2 * i + 0)) :% (indexOffAddr addr (2 * i + 1))
  readOffAddr# :: forall s a. (Prim a) => Addr# -> Int# -> State# s -> (# State# s, Ratio a #)
  readOffAddr# addr# i# = internal $ do
    let i = I# i#
        addr = Addr addr#
    a <- readOffAddr addr (2 * i + 0) :: ST s a
    b <- readOffAddr addr (2 * i + 1)
    return (a :% b)
  writeOffAddr# :: forall s a. (Prim a) => Addr# -> Int# -> Ratio a -> State# s -> State# s
  writeOffAddr# addr# i# (a :% b) = internal_ $ do
    let i = I# i#
        addr = Addr addr#
    writeOffAddr addr (2 * i + 0) a
    writeOffAddr addr (2 * i + 1) b :: ST s ()
  setOffAddr# = defaultSetOffAddr#

instance Prim Fingerprint where 
  sizeOf# _ = 2# *# sizeOf# (undefined :: Word64)
  alignment# _ = alignment# (undefined :: Word64)
  indexByteArray# arr# i# =
    let i = I# i#
        arr = ByteArray arr#
    in Fingerprint (indexByteArray arr (2 * i + 0)) (indexByteArray arr (2 * i + 1))
  readByteArray# :: forall s. MutableByteArray# s -> Int# -> State# s -> (# State# s, Fingerprint #)
  readByteArray# arr# i# = internal $ do
    let i = I# i#
        arr = MutableByteArray arr#
    a <- readByteArray arr (2 * i + 0) :: ST s Word64
    b <- readByteArray arr (2 * i + 1)
    return (Fingerprint a b)
  writeByteArray# :: forall s. MutableByteArray# s -> Int# -> Fingerprint -> State# s -> State# s
  writeByteArray# arr# i# (Fingerprint a b) = internal_ $ do
    let i = I# i#
        arr = MutableByteArray arr#
    writeByteArray arr (2 * i + 0) a
    writeByteArray arr (2 * i + 1) b :: ST s ()
  setByteArray# = defaultSetByteArray#
  indexOffAddr# :: Addr# -> Int# -> Fingerprint
  indexOffAddr# addr# i# =
    let i = I# i#
        addr = Addr addr#
    in Fingerprint (indexOffAddr addr (2 * i + 0)) (indexOffAddr addr (2 * i + 1))
  readOffAddr# :: forall s. Addr# -> Int# -> State# s -> (# State# s, Fingerprint #)
  readOffAddr# addr# i# = internal $ do
    let i = I# i#
        addr = Addr addr#
    a <- readOffAddr addr (2 * i + 0) :: ST s Word64
    b <- readOffAddr addr (2 * i + 1)
    return (Fingerprint a b)
  writeOffAddr# :: forall s. Addr# -> Int# -> Fingerprint -> State# s -> State# s
  writeOffAddr# addr# i# (Fingerprint a b) = internal_ $ do
    let i = I# i#
        addr = Addr addr#
    writeOffAddr addr (2 * i + 0) a
    writeOffAddr addr (2 * i + 1) b :: ST s ()
  setOffAddr# = defaultSetOffAddr#

deriving instance Prim a => Prim (Down a)
#if MIN_VERSION_base(4,8,0)
deriving instance Prim a => Prim (Identity a)
deriving instance Prim a => Prim (Monoid.Dual a)
deriving instance Prim a => Prim (Monoid.Sum a)
deriving instance Prim a => Prim (Monoid.Product a)
#endif
#if MIN_VERSION_base(4,9,0)
deriving instance Prim a => Prim (Semigroup.First a)
deriving instance Prim a => Prim (Semigroup.Last a)
deriving instance Prim a => Prim (Semigroup.Min a)
deriving instance Prim a => Prim (Semigroup.Max a)
#endif
deriving instance Prim a => Prim (Const a b)

internal_ :: PrimBase m => m () -> State# (PrimState m) -> State# (PrimState m)
internal_ m s = case internal m s of
  (# s', () #) -> s'
