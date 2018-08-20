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

import Data.Complex (Complex(..))
import Data.Functor.Const (Const(..))
#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity (Identity(..))
#endif
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import Data.Ord (Down(..))
--import Data.Primitive.ByteArray
import Data.Primitive.Types (Prim(..), defaultSetOffAddr#, defaultSetByteArray#)
import GHC.Real (Ratio(..))
import Data.Word (Word64)
import GHC.Fingerprint (Fingerprint(..))
import GHC.Exts (State#, Int#, Addr#, MutableByteArray#, (+#), (*#))

instance Prim a => Prim (Complex a) where
  sizeOf# _ = 2# *# sizeOf# (undefined :: a)
  alignment# _ = alignment# (undefined :: a)
  indexByteArray# arr# i# =
    let x,y :: a
        x = indexByteArray# arr# (2# *# i#)
        y = indexByteArray# arr# (2# *# i# +# 1#) 
    in x :+ y
  readByteArray# :: forall s a. (Prim a) => MutableByteArray# s -> Int# -> State# s -> (# State# s, Complex a #)
  readByteArray# arr# i# =
    \s0 -> case readByteArray# arr# (2# *# i#) s0 of
      (# s1#, x #) -> case readByteArray# arr# (2# *# i# +# 1#) s1# of
        (# s2#, y #) -> (# s2#, x :+ y #)
  writeByteArray# :: forall s a. (Prim a) => MutableByteArray# s -> Int# -> Complex a -> State# s -> State# s
  writeByteArray# arr# i# (a :+ b) =
    \s0 -> case writeByteArray# arr# (2# *# i#) a s0 of
      s1 -> case writeByteArray# arr# (2# *# i# +# 1#) b s1 of
        s2 -> s2
  setByteArray# = defaultSetByteArray#
  indexOffAddr# :: Addr# -> Int# -> Complex a
  indexOffAddr# addr# i# =
    let x,y :: a
        x = indexOffAddr# addr# (2# *# i#)
        y = indexOffAddr# addr# (2# *# i# +# 1#)
    in x :+ y
  readOffAddr# :: forall s a. (Prim a) => Addr# -> Int# -> State# s -> (# State# s, Complex a #)
  readOffAddr# addr# i# =
    \s0 -> case readOffAddr# addr# (2# *# i#) s0 of
      (# s1, x #) -> case readOffAddr# addr# (2# *# i# +# 1#) s1 of
        (# s2, y #) -> (# s2, x :+ y #)
  writeOffAddr# :: forall s a. (Prim a) => Addr# -> Int# -> Complex a -> State# s -> State# s
  writeOffAddr# addr# i# (a :+ b) =
    \s0 -> case writeOffAddr# addr# (2# *# i#) a s0 of
      s1 -> case writeOffAddr# addr# (2# *# i# +# 1#) b s1 of
        s2 -> s2
  setOffAddr# = defaultSetOffAddr#
  {-# INLINE sizeOf# #-}
  {-# INLINE alignment# #-}
  {-# INLINE indexByteArray# #-}
  {-# INLINE readByteArray# #-}
  {-# INLINE writeByteArray# #-}
  {-# INLINE setByteArray# #-}
  {-# INLINE indexOffAddr# #-}
  {-# INLINE readOffAddr# #-}
  {-# INLINE writeOffAddr# #-}
  {-# INLINE setOffAddr# #-}

instance (Integral a, Prim a) => Prim (Ratio a) where
  sizeOf# _ = 2# *# sizeOf# (undefined :: a) 
  alignment# _ = alignment# (undefined :: a) 
  indexByteArray# arr# i# =
    let x,y :: a
        x = indexByteArray# arr# (2# *# i#)
        y = indexByteArray# arr# (2# *# i# +# 1#) 
    in x :% y
  readByteArray# :: forall s a. (Prim a) => MutableByteArray# s -> Int# -> State# s -> (# State# s, Ratio a #)
  readByteArray# arr# i# =
    \s0 -> case readByteArray# arr# (2# *# i#) s0 of
      (# s1#, x #) -> case readByteArray# arr# (2# *# i# +# 1#) s1# of
        (# s2#, y #) -> (# s2#, x :% y #)
  writeByteArray# :: forall s a. (Prim a) => MutableByteArray# s -> Int# -> Ratio a -> State# s -> State# s
  writeByteArray# arr# i# (a :% b) =
    \s0 -> case writeByteArray# arr# (2# *# i#) a s0 of
      s1 -> case writeByteArray# arr# (2# *# i# +# 1#) b s1 of
        s2 -> s2
  setByteArray# = defaultSetByteArray#
  indexOffAddr# :: Addr# -> Int# -> Ratio a
  indexOffAddr# addr# i# =
    let x,y :: a
        x = indexOffAddr# addr# (2# *# i#)
        y = indexOffAddr# addr# (2# *# i# +# 1#)
    in x :% y
  readOffAddr# :: forall s a. (Prim a) => Addr# -> Int# -> State# s -> (# State# s, Ratio a #)
  readOffAddr# addr# i# =
    \s0 -> case readOffAddr# addr# (2# *# i#) s0 of
      (# s1, x #) -> case readOffAddr# addr# (2# *# i# +# 1#) s1 of
        (# s2, y #) -> (# s2, x :% y #)
  writeOffAddr# :: forall s a. (Prim a) => Addr# -> Int# -> Ratio a -> State# s -> State# s
  writeOffAddr# addr# i# (a :% b) =
    \s0 -> case writeOffAddr# addr# (2# *# i#) a s0 of
      s1 -> case writeOffAddr# addr# (2# *# i# +# 1#) b s1 of
        s2 -> s2
  setOffAddr# = defaultSetOffAddr#
  {-# INLINE sizeOf# #-}
  {-# INLINE alignment# #-}
  {-# INLINE indexByteArray# #-}
  {-# INLINE readByteArray# #-}
  {-# INLINE writeByteArray# #-}
  {-# INLINE setByteArray# #-}
  {-# INLINE indexOffAddr# #-}
  {-# INLINE readOffAddr# #-}
  {-# INLINE writeOffAddr# #-}
  {-# INLINE setOffAddr# #-}

instance Prim Fingerprint where 
  sizeOf# _ = 2# *# sizeOf# (undefined :: Word64)
  alignment# _ = alignment# (undefined :: Word64)
  indexByteArray# arr# i# =
    let x,y :: Word64
        x = indexByteArray# arr# (2# *# i#)
        y = indexByteArray# arr# (2# *# i# +# 1#) 
    in Fingerprint x y
  readByteArray# :: forall s. MutableByteArray# s -> Int# -> State# s -> (# State# s, Fingerprint #)
  readByteArray# arr# i# =
    \s0 -> case readByteArray# arr# (2# *# i#) s0 of
      (# s1#, x #) -> case readByteArray# arr# (2# *# i# +# 1#) s1# of
        (# s2#, y #) -> (# s2#, Fingerprint x y #)
  writeByteArray# :: forall s. MutableByteArray# s -> Int# -> Fingerprint -> State# s -> State# s
  writeByteArray# arr# i# (Fingerprint a b) =
    \s0 -> case writeByteArray# arr# (2# *# i#) a s0 of
      s1 -> case writeByteArray# arr# (2# *# i# +# 1#) b s1 of
        s2 -> s2
  setByteArray# = defaultSetByteArray#
  indexOffAddr# :: Addr# -> Int# -> Fingerprint
  indexOffAddr# addr# i# =
    let x,y :: Word64
        x = indexOffAddr# addr# (2# *# i#)
        y = indexOffAddr# addr# (2# *# i# +# 1#)
    in Fingerprint x y
  readOffAddr# :: forall s. Addr# -> Int# -> State# s -> (# State# s, Fingerprint #)
  readOffAddr# addr# i# =
    \s0 -> case readOffAddr# addr# (2# *# i#) s0 of
      (# s1, x #) -> case readOffAddr# addr# (2# *# i# +# 1#) s1 of
        (# s2, y #) -> (# s2, Fingerprint x y #)
  writeOffAddr# :: forall s. Addr# -> Int# -> Fingerprint -> State# s -> State# s
  writeOffAddr# addr# i# (Fingerprint a b) =
    \s0 -> case writeOffAddr# addr# (2# *# i#) a s0 of
      s1 -> case writeOffAddr# addr# (2# *# i# +# 1#) b s1 of
        s2 -> s2
  setOffAddr# = defaultSetOffAddr#
  {-# INLINE sizeOf# #-}
  {-# INLINE alignment# #-}
  {-# INLINE indexByteArray# #-}
  {-# INLINE readByteArray# #-}
  {-# INLINE writeByteArray# #-}
  {-# INLINE setByteArray# #-}
  {-# INLINE indexOffAddr# #-}
  {-# INLINE readOffAddr# #-}
  {-# INLINE writeOffAddr# #-}
  {-# INLINE setOffAddr# #-}

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