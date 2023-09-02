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
    [h, w] <- list1 ucInt
    xm <- list2 h ucInt
    q <- readLn @Int
    abcds <- list2 q ucInt

    let cum = toVec2 h w $ mkCumulative xm

    forM_ abcds $ \abcd -> do
        print $ solve cum abcd


mkCumulative :: [[Int]] -> [[Int]]
mkCumulative = L.transpose . fmap (scanl1 (+)) . L.transpose . fmap (scanl1 (+))

solve :: Vector (Vector Int) -> [Int] -> Int
solve vv [a,b,c,d] = let cd = vv !@ (c-1,d-1)
                         ab = if a<=1 || b<=1 then 0 else vv !@ (a-2,b-2)
                         ad = if a<=1 then 0 else vv !@ (a-2,d-1)
                         bc = if b<=1 then 0 else vv !@ (c-1,b-2)
                     in cd + ab - ad - bc


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
