module My.Algorithm.DFS where

import Control.Monad.Fix
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.ST
import Data.Foldable
import My.Data.Array
import My.Data.Ref
import Data.Traversable

noop :: a -> ST s ()
noop _ = pure ()

-- | 深さ優先探索
-- ex. onGraph: let dist = dfs (adj graph) (bounds graph) [1]
-- ex. onGrid:  let dist = dfs (candidates ((/='#').(g!)) bnd nei4) bnd [(1,1)]
dfs :: forall i s. Ix i =>
    (i -> ST s ()) -> -- ^ 行きがけ順の処理
    (i -> ST s ()) -> -- ^ 帰りがけ順の処理
    (i -> [i]) ->     -- ^ 現在点から探索候補点を取得
    (i, i) ->         -- ^ 探索範囲のbound
    i ->              -- ^ 開始点
    ST s ()
dfs onEnter onExit nexts b start = do

    seen <- newUArray b False

    flip fix start $ \loop v -> do
        t <- readArray seen v
        if t then return () else do
            writeArray seen v True
            onEnter v
            for_ (nexts v) loop
            onExit v

-- | DAGをトポロジカルソートして返す
-- ex. onGraph: let dist = topologicalSort (adj graph) (bounds graph)
topologicalSort :: forall i. Ix i => (i -> [i]) -> (i, i) -> [i]
topologicalSort nexts b = runST $ do
    order <- newRef []

    for_ (range b) $ dfs noop (\i -> modifyRef order (i:)) nexts b

    readRef order

-- | 無向グラフの連結成分のリストを返す
-- 各連結成分は訪問順の逆順に並んでいる
connectedComponents :: forall i. Ix i => (i -> [i]) -> (i, i) -> [[i]]
connectedComponents nexts b = filter (not . null) $ runST $ do
    for (range b) $ \s -> do
        ref <- newRef []

        dfs (\i -> modifyRef ref (i:)) noop nexts b s

        readRef ref
