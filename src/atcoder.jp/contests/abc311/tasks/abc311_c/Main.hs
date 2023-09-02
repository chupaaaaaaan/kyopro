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
import GHC.STRef (STRef(STRef))


{- | generate graph as adjacency list
 有向グラフの場合は、to側の隣接リスト追加部分をコメントアウトすれば良い
 頂点に重みがある場合は、abc 138 dなど参照
 https://atcoder.jp/contests/abc138/submissions/15808936
-}
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
        -- VM.modify g ((f, w) :) t
    return g

-- | generate graph as adjacency list without weight
genGraph' ::
    -- | number of vertex
    Int ->
    -- | tuple of edge (from-vertex, to-vertex)
    Vector (Int, Int) ->
    Vector [(Int, ())]
genGraph' n = genGraph n . V.map (\(x, y) -> (x, y, ()))



-- | execute dfs on graph.
dfs ::
    -- | list of start vertex
    [Int] ->
    -- | adjacency list
    Vector [(Int, a)] ->
    Int
dfs vs graph = runST $ do
    -- visited vertex?
    seen <- VM.replicate (V.length graph) False
    vseen <- newSTRef (-1)
    forM_ vs $ \v -> do
        v' <- readSTRef vseen
        if v' /= -1 then return () else go vseen seen v

    readSTRef vseen

  where
    go :: STRef s Int -> MVector s Bool -> Int -> ST s ()
    go vseen seen v = do
        s <- VM.read seen v
        if s
            then writeSTRef vseen v
            else do
                VM.write seen v True
                forM_ (graph V.! v) $ \(nv, _) -> go vseen seen nv

solve :: Vector Int -> Int -> [Int]
solve vec start = go [] start
    where go :: [Int] -> Int -> [Int]
          go l v = let next = vec V.! v - 1
                   in if start == next then v:l else go (v:l) next


main :: IO ()
main = do
    n <- readLn @Int

    li <- list1 ucInt
    let graph = genGraph' n .  V.fromList . zip [1..] $ li

    let vseen = dfs [0..n-1] graph
        ans = solve (V.fromList li) vseen

    print $ length ans
    putStrLn $ unwords $ reverse $ map (show .(1+)) ans


    return ()

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
