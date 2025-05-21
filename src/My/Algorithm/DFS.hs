module My.Algorithm.DFS where

import Control.Monad.Fix
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.ST
import Data.Foldable
import My.Data.Array
import My.Data.Ref

-- | 深さ優先探索
-- ex. onGraph: let dist = dfs (adj graph) (bounds graph) [1]
-- ex. onGrid:  let dist = dfs (candidates ((/='#').(g!)) bnd nei4) bnd [(1,1)]
dfs :: forall i. Ix i =>
    (i -> [i]) -> -- ^ 現在点から探索候補点を取得
    (i, i) ->     -- ^ 探索範囲のbound
    [i] ->        -- ^ 開始点
    Array i (Maybe i)
dfs nexts b start = runSTArray $ do

    -- 訪問済みかを管理するarrayを作成
    seen <- newBArray b Nothing

    -- 探索開始
    for_ start $ \s -> flip fix s $ \loop v -> do
        t <- readArray seen v
        case t of
            Just _ -> return ()
            Nothing -> do
                writeArray seen v (Just s)
                for_ (nexts v) loop

    -- 訪問状態を返却
    return seen

-- | DAGをトポロジカルソートした頂点リストを返す
-- DAGでないグラフを与えることは想定していない
-- ex. onGraph: let dist = topologicalSort (adj graph) (bounds graph)
topologicalSort :: forall i. Ix i =>
    (i -> [i]) -> -- ^ 現在点から探索候補点を取得
    (i, i) ->     -- ^ 探索範囲のbound
    [i]
topologicalSort nexts b = runST $ do

    -- 訪問済みかを管理するarrayを作成
    seen <- newUArray b False

    order <- newRef []

    -- 探索開始
    for_ (range b) $ \s -> flip fix s $ \loop v -> do
        t <- readArray seen v
        if t then return () else do
            writeArray seen v True
            for_ (nexts v) loop
            modifyRef order (v:)

    readRef order
