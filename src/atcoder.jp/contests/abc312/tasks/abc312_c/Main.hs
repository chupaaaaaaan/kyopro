{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Array.IArray
import Data.Array.ST
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.List as L
import Data.Maybe
import Data.STRef
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as VM

import Numeric
import Debug.Trace


sortedVector :: Ord a => Vector a -> Vector a
sortedVector v = V.create $ do
    mv <- V.thaw v
    sort mv
    return mv


bsearch ::
    -- | indexに対する述語
    (Int -> Bool) ->
    -- | ok: 解が存在するindex
    Int ->
    -- | ng: 解が存在しないindex
    Int ->
    Int
bsearch isOk = go
  where
    go :: Int -> Int -> Int
    go ok ng
        | abs (ok - ng) > 1 =
            let mid = (ok + ng) `div` 2
             in if isOk mid
                    then go mid ng
                    else go ok mid
        | otherwise = ok


isOk :: Int -> Int -> Vector Int -> Vector Int -> Int -> Bool
isOk n m as bs x = let aidx = bsearch (\i -> as V.! i <= x) (-1) n
                       bidx = bsearch (\j -> bs V.! j >= x) m (-1)
                   in aidx >= (m-1) - bidx


main :: IO ()
main = do
    [n,m] <- list1 ucInt

    as <- sortedVector . V.fromList <$> list1 ucInt
    bs <- sortedVector . V.fromList <$> list1 ucInt


    print $ bsearch (isOk n m as bs) (10^9+1) 0


-- Input
-- converter
ucChar :: StateT ByteString Maybe Char
ucChar = StateT BS.uncons

ucInt :: StateT ByteString Maybe Int
ucInt = StateT (BS.readInt . BS.dropWhile isSpace)

-- | read a linear data as List
list1 ::
    -- | converter
    StateT ByteString Maybe a ->
    IO [a]
list1 !st = L.unfoldr (runStateT st) <$> BS.getLine

-- | read multi line data as List
list2 ::
    -- | number of lines (height)
    Int ->
    -- | converter
    StateT ByteString Maybe a ->
    IO [[a]]
list2 !n !st = fmap (L.unfoldr (runStateT st)) <$> replicateM n BS.getLine

-- | convert singleton List to Vector
toVec1 :: [[a]] -> Vector a
toVec1 = V.fromList . fmap (\[a] -> a)

-- | read a column Vector of order n whose element is 2-tuple
toVect :: [[a]] -> Vector (a, a)
toVect = V.fromList . fmap (\[a, b] -> (a, b))

-- | read a (h * w) grid data as Array
-- We assume that 2d-index starts from (1,1)
grid ::
    -- | number of lines (height)
    Int ->
    -- | number of values per a line (width)
    Int ->
    -- | converter
    StateT ByteString Maybe a ->
    IO (Array (Int, Int) a)
grid h w st = listArray ((1, 1), (h, w)) . concat <$> list2 h st
