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
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.List as L
import Data.Maybe
import Data.Proxy
import Data.STRef
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxing as V
import qualified Data.Vector.Unboxing.Mutable as VM
import Numeric

main :: IO ()
main = do
    n <- readLn @Int
    print $ solve n

solve :: Int -> Int
solve n = sum $ map go [1 .. n -1]
  where
    go :: Int -> Int
    go i =
        let x = i
            y = n - i
            ab = length $ concat [if j * j /= x then [1, 1] else [1] | j <- takeWhile (\k -> k * k <= x) [1 ..], x `mod` j == 0]
            cd = length $ concat [if j * j /= y then [1, 1] else [1] | j <- takeWhile (\k -> k * k <= y) [1 ..], y `mod` j == 0]
         in ab * cd

-- converter
unconsChar :: StateT ByteString Maybe Char
unconsChar = StateT BS.uncons

unconsInt :: StateT ByteString Maybe Int
unconsInt = StateT (BS.readInt . BS.dropWhile isSpace)

unconsInteger :: StateT ByteString Maybe Integer
unconsInteger = StateT (BS.readInteger . BS.dropWhile isSpace)

-- read a linear data as List
rowl :: StateT ByteString Maybe a -> IO [a]
rowl !st = L.unfoldr (runStateT st) <$> BS.getLine

-- read a row Vector of order n
rowv :: Vector v a => Int -> StateT ByteString Maybe a -> IO (v a)
rowv !n !st = VG.unfoldrN n (runStateT st) <$> BS.getLine

-- read a column Vector of order n
colv :: Vector v a => Int -> StateT ByteString Maybe a -> IO (v a)
colv !n !st = VG.replicateM n (\[a] -> a) <$> rowl st

-- read a column Vector of order n whose element is 2-tuple
colv2 :: Vector v (a, a) => Int -> StateT ByteString Maybe a -> IO (v (a, a))
colv2 !n !st = VG.replicateM n $ (\[a, b] -> (a, b)) <$> rowl st

-- read a column Vector of order n whose element is 3-tuple
colv3 :: Vector v (a, a, a) => Int -> StateT ByteString Maybe a -> IO (v (a, a, a))
colv3 !n !st = VG.replicateM n $ (\[a, b, c] -> (a, b, c)) <$> rowl st

-- read a (n * m) grid data as Vector (linear-indexed)
-- n: number of lines
-- m: number of values per a line
-- example: if you want to access the element of (i, j) (0<=i<m, 0<=j<n), the index is i+m*j
gridv :: Vector v a => Int -> Int -> StateT ByteString Maybe a -> IO (v a)
gridv !n !m !st = VG.unfoldrN (n * m) (runStateT st) . BS.concat <$> replicateM n BS.getLine
