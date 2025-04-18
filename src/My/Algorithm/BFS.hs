module My.Algorithm.BFS where

import Control.Monad
import Data.Array.MArray
import Data.Sequence qualified as Sq

-- | 幅優先探索
-- ex. onGraph: (dist :: IOArray Int Int)        <- bfs (graph !) (bounds graph) [1]
-- ex. onGrid:  (dist :: IOArray (Int, Int) Int) <- bfs (filter ((/='#') . (g!)) . arounds b nei4) b [(1,1)]
bfs :: forall a m i. (MArray a Int m, Ix i) =>
    (i -> [i]) -> -- ^ 現在点から探索候補点を取得
    (i, i) ->     -- ^ 探索範囲のbound
    [i] ->        -- ^ 開始点
    m (a i Int)
bfs nexts b start = do
    -- 開始点からの距離を格納するarrayを作成（-1は訪れていないことを表す）
    dist <- newArray b (-1)
    -- 開始点に距離0を設定
    forM_ start $ \s -> writeArray dist s 0
    -- 探索実施
    go dist (Sq.fromList start)
    -- 開始点からの距離を返却
    return dist
    where
        go :: a i Int -> Sq.Seq i -> m ()
        go dist queue = case Sq.viewl queue of
            -- キューが空であれば探索終了
            Sq.EmptyL -> return ()
            -- BFS用のキューから次の探索点を取り出す
            v Sq.:< rest -> do
                -- 開始点から探索点までの距離を取得
                d <- readArray dist v
                -- 探索候補点のうち、まだ訪れていない点を列挙
                candidates <- filterM (fmap (== (-1)) . readArray dist) . nexts $ v
                -- 探索候補点に、距離d+1を設定する
                forM_ candidates $ \cand -> writeArray dist cand (d+1)
                -- 探索候補点をキューの末尾に追加し、次の探索へ
                go dist $ rest Sq.>< Sq.fromList candidates
