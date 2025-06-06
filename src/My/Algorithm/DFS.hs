module My.Algorithm.DFS where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.ST
import Data.Foldable
import Data.Traversable
import My.Data.Array
import My.Data.Ref

-- | 深さ優先探索の雛形
-- ex. onGraph: dfs (adj graph) (bounds graph) [1]
-- ex. onGrid:  dfs (nexts ((/='#').(g!)) bnd nei4) bnd [(1,1)]
dfs :: forall i s. Ix i =>
    (i -> [i]) -> -- ^ 現在点から探索候補点を取得
    (i, i) ->     -- ^ 探索範囲のbound
    [i] ->        -- ^ 開始点
    ST s ()
dfs nextStatus b start = do
    seen <- newUArray b False

    for_ start $ \s -> flip fix s $ \loop v -> do
        t <- readArray seen v
        if t then return () else do
            writeArray seen v True
            -- 行きがけ順の処理
            for_ (nextStatus v) $ \nv -> do
                -- 子ノード探索前のエッジの処理
                loop nv
                -- 子ノード探索後のエッジの処理
            -- 帰りがけ順の処理

-- | DAGをトポロジカルソートして返す
-- ex. onGraph: let dist = topologicalSort (adj graph) (bounds graph)
topologicalSort :: forall i. Ix i => (i -> [i]) -> (i, i) -> [i]
topologicalSort nextStatus b = runST $ do
    seen <- newUArray b False
    order <- newRef []

    for_ (range b) $ \start -> flip fix start $ \loop v -> do
        t <- readArray seen v
        if t then return () else do
            writeArray seen v True
            for_ (nextStatus v) loop
            modifyRef order (v:)

    readRef order

-- | 無向グラフの連結成分のリストを返す
-- 各連結成分は訪問順の逆順に並んでいる
connectedComponents :: forall i. Ix i => (i -> [i]) -> (i, i) -> [[i]]
connectedComponents nextStatus b = filter (not . null) $ runST $ do
    seen <- newUArray b False

    for (range b) $ \s -> do
        ref <- newRef []

        flip fix s $ \loop v -> do
            t <- readArray seen v
            if t then return () else do
                writeArray seen v True
                modifyRef ref (v:)
                for_ (nextStatus v) loop

        readRef ref

-- | 頂点に注目したオイラーツアーを返す
eulerTour :: forall i. Ix i => (i -> [i]) -> (i, i) -> i -> [i]
eulerTour nextStatus b start = runST $ do
    seen <- newUArray b False
    ref <- newRef []

    let f u = do
            r <- readRef ref
            when (null r || head r /= u) (modifyRef ref (u:))

    flip fix start $ \loop v -> do
        t <- readArray seen v
        if t then return () else do
            writeArray seen v True
            for_ (nextStatus v) $ \nv -> do
                f v
                loop nv
                f v

    reverse <$> readRef ref
