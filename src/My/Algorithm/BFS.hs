module My.Algorithm.BFS where

import Control.Monad
import Control.Monad.Fix
import Data.Array.ST
import Data.Array.Unboxed
import Data.Foldable
import My.Data.Array
import My.Data.Queue

-- | 幅優先探索の雛形
-- この実装では、開始点からの最小距離を管理する配列を返す
-- ex. onGraph: bfs (adj graph) (bounds graph) [1]
-- ex. onGrid:  bfs (nexts ((/='#').(g!)) bnd nei4) bnd [(1,1)]
bfs :: Ix i =>
    (i -> [i]) -> -- ^ 現在点から探索候補点を取得
    (i, i) ->     -- ^ 探索範囲のbound
    [i] ->        -- ^ 開始点
    UArray i Int
bfs nextStatus b start = runSTUArray $ do
    -- 開始点からの距離を格納するarrayを作成（-1は訪れていないことを表す）
    dist <- newUArray b (-1)

    -- 開始点に距離0を設定
    for_ start $ flip (writeArray dist) 0

    -- 探索実施
    flip fix (fromListQ start) $ \loop queue -> do
        case viewQ queue of
            EmptyQ -> return ()
            v :@ rest -> do
                d <- readArray dist v
                candidates <- filterM (fmap (== (-1)) . readArray dist) (nextStatus v)
                for_ candidates $ \cand -> writeArray dist cand (d+1)
                loop (rest `addListQ` candidates)

    -- 開始点からの距離を返却
    return dist

