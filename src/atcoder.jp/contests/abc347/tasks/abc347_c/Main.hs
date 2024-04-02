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
import Data.Array.Unboxed
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
    [n,a,b] <- list1 ucInt
    ds <- sortedVector . V.map (`mod`(a+b)) <$> vector1 n ucInt

    let dds = solve n a b ds

    putStrLn $ if dds V.! (n-1) > b then "Yes" else "No"

solve :: Int -> Int -> Int -> Vector Int -> Vector Int
solve n a b ds = V.create $ do
    u <- VM.replicate n 0

    forM_ [0..n-2] $ \i -> do
        VM.write u i (ds V.! (i+1) - ds V.! i)

    let diff =  (ds V.! 0 - ds V.! (n-1) + a + b) `mod` (a+b)

    VM.write u (n-1) (if diff == 0 then a + b else diff)

    sort u

    return u
        



-- Input
-- converter
type Conv = StateT ByteString Maybe

ucChar :: Conv Char
ucChar = StateT BS.uncons

ucInt :: Conv Int
ucInt = StateT (BS.readInt . BS.dropWhile isSpace)

ucString :: Conv ByteString
ucString = StateT (\bs -> let bs' = BS.dropWhile isSpace bs
                          in if BS.null bs'
                             then Nothing
                             else Just $ BS.break isSpace bs')

-- | read a linear data as List
list1 :: Conv a -> IO [a]
list1 !st = L.unfoldr (runStateT st) <$> BS.getLine

-- | read multi line data as List
list2 :: Int -> Conv a -> IO [[a]]
list2 !n !st = fmap (L.unfoldr (runStateT st)) <$> replicateM n BS.getLine

-- | read a line and convert to Vector
vector1 :: Int -> Conv a -> IO (Vector a)
vector1 n !st = V.unfoldrN n (runStateT st) <$> BS.getLine

toIntGrid :: (Ix a, Ix b) => ((a, b), (a, b)) -> [[Int]] -> UArray (a, b) Int
toIntGrid b = listArray b . L.concat

-- Cumulative Sum
-- | 2次元累積和を計算する。
mk2dCS :: (a -> a -> a) -> a -> [[a]] -> [[a]]
mk2dCS f a = L.transpose . fmap (L.scanl' f a) . L.transpose . fmap (L.scanl' f a)

-- | 2次元累積和をクエリする。
query2dCS :: Num c => ((a, b) -> c) -> (a, b) -> (a, b) -> c
query2dCS f (a,b) (c,d) = f (c,d) + f (a,b) - f (a,d) - f (c,b)

sortedVector :: Ord a => Vector a -> Vector a
sortedVector v = V.create $ do
    mv <- V.thaw v
    sort mv
    return mv
