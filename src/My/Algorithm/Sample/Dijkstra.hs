module My.Algorithm.Dijkstra where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Foldable
import qualified Data.IntPSQ as PSQ
import qualified Data.Sequence as Seq
import Data.Traversable
import My.Data.Array
import My.Data.Graph

-- | ダイクストラ法による最短経路探索の雛形
dijkstraSample ::
    Graph Int Int -> -- ^ 隣接リスト形式の重みつきグラフ
    Int ->            -- ^ 開始頂点
    (UArray Int Bool, UArray Int Int)
dijkstraSample graph v = runST $ do

    -- 確定済みか否かを保持する配列
    fixed <- newUArray (bounds graph) False
    -- 現時点の距離の最小値を保持する配列
    curr <- newUArray (bounds graph) maxBound

    -- 開始地点の距離を設定
    writeArray curr v 0

    -- 開始点と距離を優先度付きキュー(PSQ)に入れて探索開始
    flip fix (PSQ.singleton v 0 ()) $ \loop psq -> do
        case PSQ.findMin psq of
            -- PSQに何も残っていなければ探索終了
            Nothing -> return ()
            Just (i,c,_) -> do
                f <- readArray fixed i
                -- 確定済みであれば、PSQから最小値を取り除き次の計算へ
                if f then loop (PSQ.deleteMin psq) else do
                    -- 点を確定済みにマークする
                    writeArray fixed i True
                    -- 隣の点について、距離を更新し、優先度付きキューに追加
                    newPsq <- fmap (insertList psq) $ for (graph ! i) $ \(nv,nw) -> do
                        cw <- readArray curr nv
                        let new = min cw (c+nw)
                        writeArray curr nv new
                        return (nv,new,())
                    -- 次の計算へ
                    loop newPsq

    fixed' <- unsafeUFreeze fixed
    curr' <- unsafeUFreeze curr

    return (fixed', curr')

    where
        insertList :: Ord p => PSQ.IntPSQ p v -> [(Int, p, v)] -> PSQ.IntPSQ p v
        insertList = foldr $ \(i,p,l) q -> PSQ.insert i p l q


-- | 01-BFSの雛形
bfs01Sample :: (Ix i, Show i) =>
    (i -> [i]) -> -- ^ 現在点からのコストが0の探索候補点を取得
    (i -> [i]) -> -- ^ 現在点からのコストが1の探索候補点を取得
    (i, i) ->     -- ^ 探索範囲のbound
    [i] ->        -- ^ 開始点
    (UArray i Int, UArray i Bool)
bfs01Sample next0 next1 b start = runST $ do

    -- 確定済みか否かを保持する配列
    fixed <- newUArray b False
    -- 現時点のコストの最小値を格納する配列
    cost <- newUArray b maxBound

    -- 開始点にコスト0を設定
    for_ start $ \s -> writeArray cost s 0

    -- 探索実施
    flip fix (Seq.fromList start) $ \loop queue -> do
        case Seq.viewl queue of
            -- 両端キューが空であれば終了
            Seq.EmptyL -> return ()
            -- 両端キューの左から次の探索点を取り出す
            v Seq.:< rest -> do
                r <- readArray fixed v
                -- コストが確定している場合はスキップ
                if r then loop rest else do
                    -- 点を確定済みにマークする
                    writeArray fixed v True
                    -- 開始点から探索点までのコストを取得
                    d <- readArray cost v
                    -- コスト0の候補点についてコストを更新
                    cand0 <- filterM (fmap (>d) . readArray cost) $ next0 v
                    for_ cand0 $ \cand -> writeArray cost cand d
                    -- コスト1の候補点についてコストを更新
                    cand1 <- filterM (fmap (>d+1) . readArray cost) $ next1 v
                    for_ cand1 $ \cand -> writeArray cost cand (d+1)
                    -- コスト0、1の探索候補点をそれぞれ両端キューの左、右に追加し、次の探索へ
                    loop (Seq.fromList cand0 Seq.>< rest Seq.>< Seq.fromList cand1)

    fixed' <- unsafeUFreeze fixed
    cost' <- unsafeUFreeze cost

    return (cost', fixed')
