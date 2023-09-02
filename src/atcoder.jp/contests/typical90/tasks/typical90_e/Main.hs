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
import Data.Vector.Mutable (MVector, STVector)
import qualified Data.Vector.Mutable as VM
import Debug.Trace
import Numeric

main :: IO ()
main = do
    [n, b, k] <- rowl unconsInt
    cs <- rowv k unconsInt

    print $ solve n b k cs

solve = solve1

mtxProd1 :: Num a => Int -> Int -> Int -> Vector a -> Vector a -> Vector a
mtxProd1 h l w mtx1 mtx2 | V.length mtx1 /= h*l || V.length mtx2 /= l*w = error "invalid size of matrix vector."
                         | otherwise = let genVec :: Int -> Vector a -> Vector (Vector a)
                                           genVec n = V.unfoldrN n (\v -> if V.length v < n then Nothing else V.splitAt n v)
                                           rows = genVec h mtx1
                                           cols = V.unfoldr (\v -> Just (V.map  v, ))   $ genVec l mtx2




bjk b k cs =
    let bjk_nonZeroElems = V.concatMap (\j -> V.generate k (\k' -> (j, (10 * j + cs V.! k') `mod` b))) $ V.generate b id
        bjk_1 = V.create $ do
            bjk' <- VM.replicate (b * b) 0
            V.forM_ bjk_nonZeroElems $ \(j, k) -> VM.modify bjk' (const 1) (j + b * k)
            return bjk'
        mtxProd m1 m2 = V.create $ do
            prod <- VM.replicate (b * b) 0
            forM_ [(j, k, l) | j <- [0 .. b -1], k <- [0 .. b -1], l <- [0 .. b -1]] $ \(j, k, l) -> VM.modify prod (+ (m1 V.! (j + b * l) * m2 V.! (l + b * k))) (j + b * k)
            return prod
     in V.iterateN 5 (\x -> x `mtxProd` x) bjk_1

solve2 :: Int -> Int -> Int -> Vector Int -> Int
solve2 n b k cs =
    let dp = V.create $ do
            dp' <- VM.replicate b 0
            forM_ [0 .. k -1] $ \k' -> VM.modify dp' (1 +) (cs V.! k' `mod` b)
            return dp'
     in go 1 dp
  where
    go :: Int -> Vector Int -> Int
    go d dp
        | d == n = V.head dp
        | otherwise =
            let nexts = V.concatMap (\j -> V.generate k (\k' -> (j, (10 * j + cs V.! k') `mod` b))) $ V.generate b id
             in go (d + 1) $
                    V.create $ do
                        dp' <- VM.replicate b 0
                        forM_ nexts $ \(j, nex) -> do VM.modify dp' ((`mod` (10 ^ 9 + 7)) . (dp V.! j +)) nex
                        return dp'

solve1 :: Int -> Int -> Int -> Vector Int -> Int
solve1 n b k cs =
    let dp = V.create $ do
            dp' <- VM.replicate b 0
            forM_ [0 .. k -1] $ \k' -> VM.modify dp' (1 +) (cs V.! k' `mod` b)
            return dp'
     in go 1 dp
  where
    go :: Int -> Vector Int -> Int
    go d dp
        | d == n = V.head dp
        | otherwise =
            let nexts = V.concat $ V.toList $ V.generate b (\j -> V.generate k (\k' -> (j, (10 * j + cs V.! k') `mod` b)))
             in go (d + 1) $
                    V.create $ do
                        dp' <- VM.replicate b 0
                        forM_ nexts $ \(j, nex) -> do VM.modify dp' ((`mod` (10 ^ 9 + 7)) . (dp V.! j +)) nex
                        return dp'

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
