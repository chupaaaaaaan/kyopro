module My.Algorithm.DFS where

import Control.Monad.Fix
import Data.Array.IArray
import Data.Array.ST
import Data.Foldable
import My.Data.Array
import Data.Array.Unboxed

-- | 深さ優先探索の雛形
-- この実装では、訪問済みかを管理する配列を返す
-- ex. onGraph: dfs (adj graph) (bounds graph) [1]
-- ex. onGrid:  dfs (nexts ((/='#').(g!)) bnd nei4) bnd [(1,1)]
dfsSample :: forall i. Ix i =>
    (i -> [i]) -> -- ^ 現在点から探索候補点を取得
    (i, i) ->     -- ^ 探索範囲のbound
    [i] ->        -- ^ 開始点
    UArray i Bool
dfsSample nextStatus b start = runSTUArray $ do
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

    return seen
