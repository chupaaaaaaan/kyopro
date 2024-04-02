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
import Debug.Trace
import Data.Array.Base (STUArray(STUArray))

main :: IO ()
main = do
    [n,q] <- list1 ucInt
    xs <- list1 ucInt

    putStrLn . unwords . map show . elems $ solve n q xs

solve :: Int -> Int -> [Int] -> UArray Int Int
solve n q xs =
    let sSizeArray = runSTUArray $ do
            sSizes <- newArray_ (1,q)
            sState <- newSTRef S.empty

            forM_ (zip [1..] xs) $ \(i, x) -> do
                s <- readSTRef sState
                let s' = if S.member x s then S.delete x s else S.insert x s
                writeSTRef sState s'
                writeArray sSizes i (S.size s')

            return sSizes

        sSizeCum = runSTUArray $ ascanl (+) 0 sSizeArray

    in runSTUArray $ do
        as <- newArray (1,n) 0
        bs <- newArray (1,n) (-1) :: ST s (STUArray s Int Int)

        forM_ (zip [1..] xs) $ \(i, x) -> do
            j <- readArray bs x
            if j == (-1)
                then writeArray bs x i
                else do readArray as x >>= writeArray as x . ((sSizeCum ! (i-1) - sSizeCum ! (j-1))+)
                        writeArray bs x (-1)

        ibs <- getAssocs bs

        forM_ ibs $ \(i, j) -> when (j /= -1) $ do
            readArray as i >>= writeArray as i . ((sSizeCum ! q - sSizeCum ! (j-1))+)

        return as
        

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
-- | Array用のscanl
ascanl :: (MArray am e1 (ST s), IArray ai e2) => (e1 -> e2 -> e1) -> e1 -> ai Int e2 -> ST s (am Int e1)
ascanl f a ary = do
    let b@(l,h) = bounds ary

    result <- newArray_ (l-1,h)

    writeArray result (l-1) a

    forM_ (range b) $ \i -> do
        x <- readArray result (i-1)
        let y = ary ! i
        writeArray result i (x `f` y)

    return result
