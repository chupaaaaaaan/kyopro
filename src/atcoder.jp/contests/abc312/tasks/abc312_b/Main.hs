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

main :: IO ()
main = do
    [n,m] <- list1 ucInt
    sgrid <- grid n m ucChar

    let solved = solve n m sgrid

    if null solved then putStrLn "" else putStr $ unlines $ map (\(x,y) -> show x <> " " <> show y) $ solved

solve :: Int -> Int -> Array (Int, Int) Char -> [(Int,Int)]
solve n m sg = catMaybes [go i j | i <- [1..n-8], j <- [1..m-8]]
    where go :: Int -> Int -> Maybe (Int, Int)
          go i j = if sg ! (i,j) == '#' &&
                      sg ! (i,j+1) == '#' &&
                      sg ! (i,j+2) == '#' &&
                      sg ! (i+1,j) == '#' &&
                      sg ! (i+1,j+1) == '#' &&
                      sg ! (i+1,j+2) == '#' &&
                      sg ! (i+2,j) == '#' &&
                      sg ! (i+2,j+1) == '#' &&
                      sg ! (i+2,j+2) == '#' &&

                      sg ! (i+3,j) == '.' &&
                      sg ! (i+3,j+1) == '.' &&
                      sg ! (i+3,j+2) == '.' &&
                      sg ! (i+3,j+3) == '.' &&
                      sg ! (i+2,j+3) == '.' &&
                      sg ! (i+1,j+3) == '.' &&
                      sg ! (i,j+3) == '.' &&

                      sg ! (i+6,j+6) == '#' &&
                      sg ! (i+6,j+7) == '#' &&
                      sg ! (i+6,j+8) == '#' &&
                      sg ! (i+7,j+6) == '#' &&
                      sg ! (i+7,j+7) == '#' &&
                      sg ! (i+7,j+8) == '#' &&
                      sg ! (i+8,j+6) == '#' &&
                      sg ! (i+8,j+7) == '#' &&
                      sg ! (i+8,j+8) == '#' &&

                      sg ! (i+5,j+8) == '.' &&
                      sg ! (i+5,j+7) == '.' &&
                      sg ! (i+5,j+6) == '.' &&
                      sg ! (i+5,j+5) == '.' &&
                      sg ! (i+6,j+5) == '.' &&
                      sg ! (i+7,j+5) == '.' &&
                      sg ! (i+8,j+5) == '.'
                   then Just (i,j)
                   else Nothing


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
