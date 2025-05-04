module My.Algorithm.DFS where

import Control.Monad.Fix
import Data.Array.MArray
import Data.Foldable

-- | 深さ優先探索
-- ex. onGraph: (dist :: IOArray Int Int)        <- dfs (graph !) (bounds graph) [1]
-- ex. onGrid:  (dist :: IOArray (Int, Int) Int) <- dfs (candidates ((/='#').(g!)) bnd nei4) bnd [(1,1)]
dfs :: forall a m i. (MArray a (Maybe i) m, Ix i) =>
    (i -> [i]) -> -- ^ 現在点から探索候補点を取得
    (i, i) ->     -- ^ 探索範囲のbound
    [i] ->        -- ^ 開始点
    m (a i (Maybe i))
dfs nexts b start = do

    -- 訪問済みかを管理するarrayを作成
    seen <- newArray b Nothing

    -- 探索開始
    forM_ start $ \s -> flip fix s $ \loop v -> do
        t <- readArray seen v
        case t of
            Just _ -> return ()
            Nothing -> do
                writeArray seen v (Just s)
                forM_ (nexts v) loop

    -- 訪問状態を返却
    return seen
