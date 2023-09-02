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
    as <- list1 ucChar

    putStrLn $ solve n as

    return ()


solve :: Int -> [Char] -> [Char]
solve n ss = let periods = L.sort $ go [] $ zip [1..n] ss
              in fo 0 ss periods
    where go :: [Int] -> [(Int, Char)] -> [(Int, Int)]
          go _ [] = []
          go xs ((x,t):ts) | t == '(' = go (x:xs) ts
                           | t == ')' = if null xs
                                        then error "don't come here"
                                        else (head xs, x) : go (tail xs) ts
                           | otherwise = go xs ts
          fo :: Int -> [Char] -> [(Int, Int)] -> [Char]
          fo _ [] _ = []
          fo _ _ [] = []
          fo n as ((l,r):lrs) | n >= l = fo n as lrs
                              | otherwise = let pre = take (l-n-1) as
                                                era = drop (r-n) as
                                            in pre <> fo r era lrs


-- Input
-- converter
ucChar :: StateT ByteString Maybe Char
ucChar = StateT BS.uncons

ucInt :: StateT ByteString Maybe Int
ucInt = StateT (BS.readInt . BS.dropWhile isSpace)

-- read a linear data as List
list1 :: StateT ByteString Maybe a -> IO [a]
list1 !st = L.unfoldr (runStateT st) <$> BS.getLine

list2 :: Int -> StateT ByteString Maybe a -> IO [[a]]
list2 !n !st = fmap (L.unfoldr (runStateT st)) <$> replicateM n BS.getLine

-- read a column Vector of order n
vec1 :: Int -> StateT ByteString Maybe a -> IO (Vector a)
vec1 !n !st = toVec1 n <$> list2 n st

toVec1 :: Int -> [[a]] -> Vector a
toVec1 !n = V.fromListN n . fmap (\[a] -> a)

-- read a column Vector of order n whose element is 2-tuple
toVect :: Int -> [[a]] -> Vector (a, a)
toVect !n = V.fromList . fmap (\[a, b] -> (a, b))

-- read a (h * w) grid data as Vector (linear-indexed)
-- n: number of lines (height)
-- m: number of values per a line (width)
-- example: if you want to access the element of (h, w) (0<=i<h, 0<=j<w), the index is i*w+j.
vec2 :: Int -> Int -> StateT ByteString Maybe a -> IO (Vector (Vector a))
vec2 !h !w !st = toVec2 h w <$> list2 h st

toVec2 :: Int -> Int -> [[a]] -> Vector (Vector a)
toVec2 !h !w = V.fromListN h . fmap (V.fromListN w)

infixl 9 !@
(!@) :: Vector (Vector a) -> (Int , Int) -> a
vv !@ (i, j) = vv V.! i V.! j
