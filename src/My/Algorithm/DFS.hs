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

    return seen
