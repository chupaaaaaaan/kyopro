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
    n <- readLn @Int
    as <- grid n n ucChar

    let bs = solve n as

    forM_ [1..n] $ \i -> do
        forM_ [1..n] $ \j -> do
            putStr [bs ! (i,j)]
        putStr "\n"

solve :: Int -> Array (Int, Int) a -> Array (Int, Int) a
solve n as = runSTArray $ do
    
    bs <- newArray_ ((1,1), (n,n))

    forM_ [(i,j)|i<-[1..n], j<-[1..n]] $ \(i,j) -> do
        if | i == 1 && j == n -> writeArray bs (2,n) $ as ! (i,j)
           | i == n && j == 1 -> writeArray bs (n-1,1) $ as ! (i,j)
           | otherwise -> if | i == 1    -> writeArray bs (1,j+1) $ as ! (i,j)
                             | i == n    -> writeArray bs (n,j-1) $ as ! (i,j)
                             | j == 1    -> writeArray bs (i-1,1) $ as ! (i,j)
                             | j == n    -> writeArray bs (i+1,n) $ as ! (i,j)
                             | otherwise -> writeArray bs (i,j) $ as ! (i,j)
           
    return bs


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
