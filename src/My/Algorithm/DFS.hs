module My.Algorithm.DFS where

import Data.Array.MArray
import Data.Foldable

dfs :: forall a m i. (MArray a (Maybe i) m, Ix i) =>
    -- | 現在点から探索候補点を取得
    (i -> [i]) ->
    -- | 探索範囲のbound
    (i, i) ->
    -- | 開始点
    [i] ->
    m (a i (Maybe i))
dfs nexts b start = do
    -- 訪問済みかを管理するarrayを作成
    seen <- newArray b Nothing
    -- 探索開始
    forM_ start $ \s -> go seen s s
    -- 訪問状態を返却
    return seen
    where
        go :: a i (Maybe i) -> i -> i -> m ()
        go seen x v = do
            s <- readArray seen v
            case s of
                Just _ -> return ()
                Nothing -> do
                    -- 行きがけ順の処理の記載箇所
                    writeArray seen v (Just x)
                    forM_ (nexts v) $ \nv -> do
                        -- 隣接ノード毎の行きがけ順の処理の記載箇所
                        go seen x nv
                        -- 隣接ノード毎の帰りがけ順の処理の記載箇所
                    -- 帰りがけ順の処理の記載箇所
