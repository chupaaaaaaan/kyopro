module My.Algorithm.BFS where

import Control.Monad
import Control.Monad.Fix
import Data.Array.MArray
import My.Data.Queue

-- | 幅優先探索
-- ex. onGraph: dist <- bfs @IOUArray (adj graph) (bounds graph) [1]
-- ex. onGrid:  dist <- bfs @IOUArray (nexts ((/='#').(g!)) bnd nei4) bnd [(1,1)]
bfs :: (MArray a Int m, Ix i) =>
    (i -> [i]) -> -- ^ 現在点から探索候補点を取得
    (i, i) ->     -- ^ 探索範囲のbound
    [i] ->        -- ^ 開始点
    m (a i Int)
bfs nextStatus b start = do
    -- 開始点からの距離を格納するarrayを作成（-1は訪れていないことを表す）
    dist <- newArray b (-1)

    -- 開始点に距離0を設定
    forM_ start $ flip (writeArray dist) 0

    -- 探索実施
    flip fix (fromListQ start) $ \loop queue -> do
        case viewQ queue of
            EmptyQ -> return ()
            v :@ rest -> do
                d <- readArray dist v
                candidates <- filterM (fmap (== (-1)) . readArray dist) (nextStatus v)
                forM_ candidates $ \cand -> writeArray dist cand (d+1)
                loop (rest `addListQ` candidates)

    -- 開始点からの距離を返却
    return dist

