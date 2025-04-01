module My.Algorithm.Dijkstra where

import Data.Array.IArray
import Data.Array.MArray
import Data.IntPSQ qualified as PSQ
import Data.Traversable
import My.Data.Graph


-- | ダイクストラ法による最短経路探索
dijkstra :: forall a m . (MArray a Int m, MArray a Bool m) =>
    -- | 隣接リスト形式の重みつきグラフ
    WGraph Int Int ->
    -- | 開始頂点
    Int ->
    m (a Int Bool, a Int Int)
dijkstra graph v = do

    -- 確定済みか否かを保持する配列
    fixed <- newArray (bounds graph) False
    -- 現時点の距離の最小値を保持する配列
    curr <- newArray (bounds graph) maxBound

    -- 開始地点の距離を設定
    writeArray curr v 0

    -- 開始点と距離を優先度付きキュー(PSQ)に入れて探索開始
    go fixed curr $ PSQ.singleton v 0 ()
    return (fixed, curr)

    where
        go :: a Int Bool -> a Int Int -> PSQ.IntPSQ Int () -> m ()
        go fixed curr psq  = do
            case PSQ.findMin psq of
                -- PSQに何も残っていなければ探索終了
                Nothing -> return ()
                Just (i,c,_) -> do
                    f <- readArray fixed i
                    -- 確定済みであれば、PSQから最小値を取り除き次の計算へ
                    if f then go fixed curr (PSQ.deleteMin psq) else do
                        -- 点を確定済みにマークする
                        writeArray fixed i True
                        -- 隣の点について、距離を更新し、優先度付きキューに追加
                        newPsq <- fmap (insertList psq) $ forM (graph ! i) $ \(nv,nw) -> do
                            cw <- readArray curr nv
                            let new = min cw (c+nw)
                            writeArray curr nv new
                            return (nv,new,())
                        -- 次の計算へ
                        go fixed curr newPsq
        insertList :: Ord p => PSQ.IntPSQ p v -> [(Int, p, v)] -> PSQ.IntPSQ p v
        insertList = foldr $ \(i,p,l) q -> PSQ.insert i p l q
