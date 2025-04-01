{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module My.Data.Graph where

import Data.Array.IArray
import Data.Array.ST
import Data.Foldable
import My.Data.Array

type Graph i = Array i [i]
type WGraph i a = Array i [(i, a)]

-- | 隣接リスト形式の重み付きグラフを生成する
-- 頂点に重みがある場合は、abc 138 Dなど参照（https://atcoder.jp/contests/abc138/submissions/15808936 ）
genWeightedGraph, genWeightedDigraph :: Ix i =>
    (i, i) ->      -- ^ 頂点の範囲
    [(i, i, a)] -> -- ^ 辺のリスト (開始, 終了, 重み)
    WGraph i a

-- | 無向グラフ
genWeightedGraph b edges = runSTArray $ do
    g <- newArray b []
    forM_ edges $ \(f,t,w) -> do
        modifyArray g f ((t,w):)
        modifyArray g t ((f,w):)
    return g

-- | 有向グラフ
genWeightedDigraph b edges = runSTArray $ do
    g <- newArray b []
    forM_ edges $ \(f,t,w) -> modifyArray g f ((t,w):)
    return g


-- | 隣接リスト形式の重みなしグラフを生成する
genGraph, genDigraph :: Ix i =>
    (i, i) ->   -- ^ 頂点の範囲
    [(i, i)] -> -- ^ 辺のリスト (開始, 終了)
    Graph i

-- | 無向グラフ
genGraph b edges = runSTArray $ do
    g <- newArray b []
    forM_ edges $ \(f,t) -> do
        modifyArray g f (t:)
        modifyArray g t (f:)
    return g

-- | 有向グラフ
genDigraph b edges = runSTArray $ do
    g <- newArray b []
    forM_ edges $ \(f,t) -> modifyArray g f (t:)
    return g
