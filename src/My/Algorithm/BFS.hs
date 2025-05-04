module My.Algorithm.BFS where

import Control.Monad
import Control.Monad.Fix
import Data.Array.MArray
import qualified Data.Sequence as Seq

-- | 幅優先探索
-- ex. onGraph: (dist :: IOArray Int Int)        <- bfs (graph !) (bounds graph) [1]
-- ex. onGrid:  (dist :: IOArray (Int, Int) Int) <- bfs (candidates ((/='#').(g!)) bnd nei4) bnd [(1,1)]
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
    flip fix (Seq.fromList start) $ \loop queue -> do
        case Seq.viewl queue of
            -- キューが空であれば探索終了
            Seq.EmptyL -> return ()
            -- BFS用のキューから次の探索点を取り出す
            v Seq.:< rest -> do
                -- 開始点から探索点までの距離を取得
                d <- readArray dist v
                -- 探索候補点のうち、まだ訪れていない点を列挙
                candidates <- filterM (fmap (== (-1)) . readArray dist) . nexts $ v
                -- 探索候補点に、距離d+1を設定する
                forM_ candidates $ \cand -> writeArray dist cand (d+1)
                -- 探索候補点をキューの末尾に追加し、次の探索へ
                loop (rest Seq.>< Seq.fromList candidates)

    -- 開始点からの距離を返却
    return dist
