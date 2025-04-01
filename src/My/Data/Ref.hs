{-# LANGUAGE FunctionalDependencies #-}
module My.Data.Ref where

import Data.IORef
import Data.STRef
import Control.Monad.ST

class Monad m => Ref r m | m -> r where
    newRef :: a -> m (r a)
    readRef :: r a -> m a
    writeRef :: r a -> a -> m ()
    modifyRef :: r a -> (a -> a) -> m ()
    modifyRef' :: r a -> (a -> a) -> m ()

instance Ref IORef IO where
    newRef = newIORef
    readRef = readIORef
    writeRef = writeIORef
    modifyRef = modifyIORef
    modifyRef' = modifyIORef'

instance Ref (STRef s) (ST s) where
    newRef = newSTRef
    readRef = readSTRef
    writeRef = writeSTRef
    modifyRef = modifySTRef
    modifyRef' = modifySTRef'

