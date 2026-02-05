{-# LANGUAGE FunctionalDependencies #-}
module My.Data.Array where

import Control.Monad.ST
import Data.Array.IArray
import Data.Array.IO
import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Unsafe

class (Monad m, MArray a e m) => MBArray a e m | m -> a where
    newBArray :: Ix i => (i, i) -> e -> m (a i e)
    newBArray_ :: Ix i => (i, i) -> m (a i e)
    newBListArray :: Ix i => (i, i) -> [e] -> m (a i e)
    unsafeBFreeze :: (Ix i, MArray a e m) => a i e -> m (Array i e)
    unsafeBThaw :: (Ix i, MArray a e m) => Array i e -> m (a i e)

instance MBArray IOArray e IO where
    newBArray = newArray
    newBArray_ = newArray_
    newBListArray = newListArray
    unsafeBFreeze = unsafeFreeze
    unsafeBThaw = unsafeThaw

instance MBArray (STArray s) e (ST s) where
    newBArray = newArray
    newBArray_ = newArray_
    newBListArray = newListArray
    unsafeBFreeze = unsafeFreeze
    unsafeBThaw = unsafeThaw

class (Monad m, MArray a e m) => MUArray a e m | m -> a where
    newUArray :: Ix i => (i, i) -> e -> m (a i e)
    newUArray_ :: Ix i => (i, i) -> m (a i e)
    newUListArray :: Ix i => (i, i) -> [e] -> m (a i e)
    unsafeUFreeze :: (Ix i, MArray a e m) => a i e -> m (UArray i e)
    unsafeUThaw :: (Ix i, MArray a e m) => UArray i e -> m (a i e)

instance MUArray IOUArray Bool IO where
  newUArray = newArray
  newUArray_ = newArray_
  newUListArray = newListArray
  unsafeUFreeze = unsafeFreeze
  unsafeUThaw = unsafeThaw
instance MUArray IOUArray Char IO where
  newUArray = newArray
  newUArray_ = newArray_
  newUListArray = newListArray
  unsafeUFreeze = unsafeFreeze
  unsafeUThaw = unsafeThaw
instance MUArray IOUArray Double IO where
  newUArray = newArray
  newUArray_ = newArray_
  newUListArray = newListArray
  unsafeUFreeze = unsafeFreeze
  unsafeUThaw = unsafeThaw
instance MUArray IOUArray Float IO where
  newUArray = newArray
  newUArray_ = newArray_
  newUListArray = newListArray
  unsafeUFreeze = unsafeFreeze
  unsafeUThaw = unsafeThaw
instance MUArray IOUArray Int IO where
  newUArray = newArray
  newUArray_ = newArray_
  newUListArray = newListArray
  unsafeUFreeze = unsafeFreeze
  unsafeUThaw = unsafeThaw
instance MUArray IOUArray Word IO where
  newUArray = newArray
  newUArray_ = newArray_
  newUListArray = newListArray
  unsafeUFreeze = unsafeFreeze
  unsafeUThaw = unsafeThaw

instance MUArray (STUArray s) Bool (ST s) where
  newUArray = newArray
  newUArray_ = newArray_
  newUListArray = newListArray
  unsafeUFreeze = unsafeFreeze
  unsafeUThaw = unsafeThaw
instance MUArray (STUArray s) Char (ST s) where
  newUArray = newArray
  newUArray_ = newArray_
  newUListArray = newListArray
  unsafeUFreeze = unsafeFreeze
  unsafeUThaw = unsafeThaw
instance MUArray (STUArray s) Double (ST s) where
  newUArray = newArray
  newUArray_ = newArray_
  newUListArray = newListArray
  unsafeUFreeze = unsafeFreeze
  unsafeUThaw = unsafeThaw
instance MUArray (STUArray s) Float (ST s) where
  newUArray = newArray
  newUArray_ = newArray_
  newUListArray = newListArray
  unsafeUFreeze = unsafeFreeze
  unsafeUThaw = unsafeThaw
instance MUArray (STUArray s) Int (ST s) where
  newUArray = newArray
  newUArray_ = newArray_
  newUListArray = newListArray
  unsafeUFreeze = unsafeFreeze
  unsafeUThaw = unsafeThaw
instance MUArray (STUArray s) Word (ST s) where
  newUArray = newArray
  newUArray_ = newArray_
  newUListArray = newListArray
  unsafeUFreeze = unsafeFreeze
  unsafeUThaw = unsafeThaw
