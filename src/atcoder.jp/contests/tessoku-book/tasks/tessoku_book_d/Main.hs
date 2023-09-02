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
import Data.STRef
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as VM
import GHC.STRef
import Numeric

main :: IO ()
main = do
    n <- readLn @Int

    putStrLn $ solve n

    return ()

solve :: Int -> String
solve n = let raw = reverse $ go n
          in replicate (max (10 - length raw) 0) '0' <> raw
    where go :: Int -> String
          go 0 = []
          go n = show (n`mod`2) <> go (n`div`2)





-- Input
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
rowv :: Int -> StateT ByteString Maybe a -> IO (Vector a)
rowv !n !st = V.unfoldrN n (runStateT st) <$> BS.getLine

-- read a column Vector of order n
colv :: Int -> StateT ByteString Maybe a -> IO (Vector a)
colv !n !st = V.replicateM n $ (\[a] -> a) <$> rowl st

-- read a column Vector of order n whose element is 2-tuple
colv2 :: Int -> StateT ByteString Maybe a -> IO (Vector (a, a))
colv2 !n !st = V.replicateM n $ (\[a, b] -> (a, b)) <$> rowl st

-- read a column Vector of order n whose element is 3-tuple
colv3 :: Int -> StateT ByteString Maybe a -> IO (Vector (a, a, a))
colv3 !n !st = V.replicateM n $ (\[a, b, c] -> (a, b, c)) <$> rowl st

-- read a (h * w) grid data as Vector (linear-indexed)
-- n: number of lines (height)
-- m: number of values per a line (width)
-- example: if you want to access the element of (h, w) (0<=i<h, 0<=j<w), the index is i*w+j.
gridv :: Int -> Int -> StateT ByteString Maybe a -> IO (Vector a)
gridv !h !w !st = V.concatMap (V.unfoldrN w (runStateT st)) <$> V.replicateM h BS.getLine
