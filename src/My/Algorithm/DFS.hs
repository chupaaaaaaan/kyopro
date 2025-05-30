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
import My.Misc

noop :: a -> ST s ()
noop _ = pure ()

data EdgeType i = TreeEdge i i
                | BackEdge i i
                | ForwardOrCrossEdge i i

intToEdgeType :: Ix i => Int -> i -> i -> EdgeType i
intToEdgeType x u v= case x of
    0 -> TreeEdge u v
    1 -> BackEdge u v
    2 -> ForwardOrCrossEdge u v
    _ -> notComeHere

edgeFrom :: EdgeType i -> (i, i)
edgeFrom (TreeEdge u v) = (u, v)
edgeFrom (BackEdge u v) = (u, v)
edgeFrom (ForwardOrCrossEdge u v) = (u, v)

-- | 深さ優先探索
-- ex. onGraph: dfs (adj graph) (bounds graph) [1]
-- ex. onGrid:  dfs (nexts ((/='#').(g!)) bnd nei4) bnd [(1,1)]
dfs :: forall i s. Ix i =>
    (i -> ST s ()) -> -- ^ 行きがけ順の処理
    (EdgeType i -> ST s ()) -> -- ^ 子ノード探索前のエッジの処理
    (EdgeType i -> ST s ()) -> -- ^ 子ノード探索後のエッジの処理
    (i -> ST s ()) -> -- ^ 帰りがけ順の処理
    (i -> [i]) ->     -- ^ 現在点から探索候補点を取得
    (i, i) ->         -- ^ 探索範囲のbound
    i ->              -- ^ 開始点
    ST s ()
dfs onEnter onEdgePre onEdgePost onExit nextStatus b start = do

    -- 0 :: 未訪問
    -- 1 :: 訪問中
    -- 2 :: 訪問済み
    seen <- newUArray b (0::Int)

    flip fix start $ \loop v -> do
        t <- readArray seen v
        if t == 2 then return () else do
            writeArray seen v 1
            onEnter v

            for_ (nextStatus v) $ \nv -> do
                stV <- readArray seen nv
                onEdgePre (intToEdgeType stV v nv)
                when (stV == 0) $ loop nv
                onEdgePost (intToEdgeType stV v nv)

            writeArray seen v 2
            onExit v

-- | DAGをトポロジカルソートして返す
-- ex. onGraph: let dist = topologicalSort (adj graph) (bounds graph)
topologicalSort :: forall i. Ix i => (i -> [i]) -> (i, i) -> [i]
topologicalSort nextStatus b = runST $ do
    order <- newRef []

    for_ (range b) $ dfs noop noop noop (\i -> modifyRef order (i:)) nextStatus b

    readRef order

-- | 無向グラフの連結成分のリストを返す
-- 各連結成分は訪問順の逆順に並んでいる
connectedComponents :: forall i. Ix i => (i -> [i]) -> (i, i) -> [[i]]
connectedComponents nextStatus b = filter (not . null) $ runST $ do
    for (range b) $ \s -> do
        ref <- newRef []

        dfs (\i -> modifyRef ref (i:)) noop noop noop nextStatus b s

        readRef ref

-- | 頂点に注目したオイラーツアーを返す
eulerTour :: forall i. Ix i => (i -> [i]) -> (i, i) -> i -> [i]
eulerTour nextStatus b start = runST $ do

    ref <- newRef []

    let f et = do
            let u = fst (edgeFrom et)
            r <- readRef ref
            when (null r || head r /= u) (modifyRef ref (u:))

    dfs noop f f noop nextStatus b start

    reverse <$> readRef ref
