module My.Algorithm.DFS where

import Data.Array.MArray
import Data.Foldable
import My.Data.Ref
import Control.Monad
import Data.Maybe


dfs :: forall a m i r. (MArray a (Maybe i) m, Ix i, Ref r m) =>
    -- | 現在点から探索候補点を取得
    (i -> [i]) ->
    -- | 探索範囲のbound
    (i, i) ->
    -- | 開始点
    [i] ->
    m (a i (Maybe i), Int)
dfs nexts b start = do
    -- 訪問済みかを管理するarrayを作成
    seen <- newArray b Nothing
    ref <- newRef 0
    -- 探索開始
    forM_ start $ \s -> do
        r <- readArray seen s
        when (isNothing r) $ go seen ref s s
    -- 訪問状態を返却
    result <- readRef ref
    return (seen, result)
    where
        go :: a i (Maybe i) -> r Int -> i -> i -> m ()
        go seen ref x v = do
            s <- readArray seen v
            case s of
                Just k -> when (x == k) $ modifyRef' ref (+1)
                Nothing -> do
                    -- 行きがけ順の処理
                    writeArray seen v (Just x)
                    forM_ (nexts v) $ \nv -> do
                        -- 隣接ノード毎の行きがけ順の処理
                        go seen ref x nv
                        -- 隣接ノード毎の帰りがけ順の処理
                    -- 帰りがけ順の処理
