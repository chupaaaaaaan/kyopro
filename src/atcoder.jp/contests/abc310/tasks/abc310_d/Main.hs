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

genGraph ::
    -- | number of vertex
    Int ->
    -- | tuple of edge (from-vertex, to-vertex, weight)
    Vector (Int, Int, a) ->
    Vector [(Int, a)]
genGraph n vec = V.create $ do
    g <- VM.replicate n []
    V.forM_ vec $ \(from, to, w) -> do
        let f = from - 1
            t = to - 1
        VM.modify g ((t, w) :) f
        VM.modify g ((f, w) :) t
    return g

-- | generate graph as adjacency list without weight
genGraph' ::
    -- | number of vertex
    Int ->
    -- | tuple of edge (from-vertex, to-vertex)
    Vector (Int, Int) ->
    Vector [(Int, ())]
genGraph' n = genGraph n . V.map (\(x, y) -> (x, y, ()))


main :: IO ()
main = do

    [n,t,m] <- list1 ucInt
    graph <- genGraph' n . toVect <$> list2 m ucInt

    

    

    return ()

-- solve :: Int -> Int -> Int -> Vector [(Int, ())] -> Int
-- solve n t m graph = 



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
