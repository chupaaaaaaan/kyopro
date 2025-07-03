{-# LANGUAGE FunctionalDependencies #-}
module My.Data.Array where

import Control.Monad.ST
import Data.Array.IArray
import Data.Array.IO
import Data.Array.ST
import Data.Foldable

class (Monad m, MArray a e m) => MBArray a e m | m -> a where
    newBArray :: Ix i => (i, i) -> e -> m (a i e)
    newBListArray :: Ix i => (i, i) -> [e] -> m (a i e)

instance MBArray IOArray e IO where
    newBArray = newArray
    newBListArray = newListArray

instance MBArray (STArray s) e (ST s) where
    newBArray = newArray
    newBListArray = newListArray

class (Monad m, MArray a e m) => MUArray a e m | m -> a where
    newUArray :: Ix i => (i, i) -> e -> m (a i e)
    newUListArray :: Ix i => (i, i) -> [e] -> m (a i e)

instance MUArray IOUArray Bool IO where
  newUArray = newArray
  newUListArray = newListArray
instance MUArray IOUArray Char IO where
  newUArray = newArray
  newUListArray = newListArray
instance MUArray IOUArray Double IO where
  newUArray = newArray
  newUListArray = newListArray
instance MUArray IOUArray Float IO where
  newUArray = newArray
  newUListArray = newListArray
instance MUArray IOUArray Int IO where
  newUArray = newArray
  newUListArray = newListArray
instance MUArray IOUArray Word IO where
  newUArray = newArray
  newUListArray = newListArray

instance MUArray (STUArray s) Bool (ST s) where
  newUArray = newArray
  newUListArray = newListArray
instance MUArray (STUArray s) Char (ST s) where
  newUArray = newArray
  newUListArray = newListArray
instance MUArray (STUArray s) Double (ST s) where
  newUArray = newArray
  newUListArray = newListArray
instance MUArray (STUArray s) Float (ST s) where
  newUArray = newArray
  newUListArray = newListArray
instance MUArray (STUArray s) Int (ST s) where
  newUArray = newArray
  newUListArray = newListArray
instance MUArray (STUArray s) Word (ST s) where
  newUArray = newArray
  newUListArray = newListArray


modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray marr i f = readArray marr i >>= writeArray marr i . f

-- | 2次元Array用のscanl
ascanl2dBase :: (MArray a e m, IArray b e) => (e -> e -> e) -> e -> b (Int, Int) e -> m (a (Int, Int) e)
ascanl2dBase f a arr = do
    let b@((li,lj),(hi,hj)) = bounds arr
        bx = ((li-1,lj-1),(hi,hj))

    result <- newArray bx a

    for_ (range b) $ \(i,j) ->
        writeArray result (i,j) $ arr ! (i,j)

    for_ [(i,j) | i <- [li..hi], j <- [lj..hj]] $ \(i,j) -> do
        x <- readArray result (i-1,j)
        y <- readArray result (i,j)
        writeArray result (i,j) (x `f` y)

    for_ [(i,j) | i <- [li..hi], j <- [lj..hj]] $ \(i,j) -> do
        x <- readArray result (i,j-1)
        y <- readArray result (i,j)
        writeArray result (i,j) (x `f` y)

    return result

-- | 2次元Array用のscanl1
ascanl2d1 :: (MArray ma e m, IArray ia e) => (e -> e -> e) -> ia (Int, Int) e -> m (ma (Int, Int) e)
ascanl2d1 f arr = do
    let b@((li,lj),(hi,hj)) = bounds arr

    result <- newArray_ b

    for_ (range b) $ \(i,j) ->
        writeArray result (i,j) $ arr ! (i,j)

    for_ [(i,j) | i <- [li+1..hi], j <- [lj..hj]] $ \(i,j) -> do
        x <- readArray result (i-1,j)
        y <- readArray result (i,j)
        writeArray result (i,j) (x `f` y)

    for_ [(i,j) | i <- [li..hi], j <- [lj+1..hj]] $ \(i,j) -> do
        x <- readArray result (i,j-1)
        y <- readArray result (i,j)
        writeArray result (i,j) (x `f` y)

    return result


-- | 2次元累積和から矩形領域の和を計算する際の抽象
rectangleSum :: (Ix a, Ix b, Num a, Num b, Num c) => ((a, b) -> c) -> (a, b) -> (a, b) -> c
rectangleSum f (a,b) (c,d) = f (c,d) + f (a-1,b-1) - f (a-1,d) - f (c,b-1)

-- | 2次元累積和をクエリする
query2dCS :: (IArray a e, Num e) => a (Int, Int) e -> (Int, Int) -> (Int, Int) -> e
query2dCS csArray = rectangleSum (csArray !)
