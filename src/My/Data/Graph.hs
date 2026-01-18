{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf #-}
module My.Data.Graph where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Foldable
import Data.Traversable
import My.Data.Array
import My.Data.Queue
import My.Data.Ref

type Graph i a = Array i [(i, a)]

class BuildEdge x i a | x -> i a where
    fAdj :: x -> [(i, (i, a))]
    bAdj :: x -> [(i, (i, a))]
    fbAdj :: x -> [(i, (i, a))]
    fbAdj x = fAdj x <> bAdj x

instance BuildEdge (i, i, a) i a where
    fAdj (p, q, a) = [(p, (q, a))]
    bAdj (p, q, a) = [(q, (p, a))]

instance BuildEdge (i, i) i () where
    fAdj (p, q) = [(p, (q, ()))]
    bAdj (p, q) = [(q, (p, ()))]

-- | 辺リストからグラフを生成する
mkGraphWith :: (Ix i, BuildEdge x i a) =>
    (x -> [(i, (i, a))]) -> -- ^ 辺の変換関数
    (i, i) ->               -- ^ 頂点の範囲
    [x] ->                  -- ^ 重み付き辺リスト
    Graph i a
mkGraphWith f b = accumArray (flip (:)) [] b . concatMap f

-- | 矢印の向きを逆にしたグラフを生成する
-- DAGでDPする際の頂点リストとしても使用する
transposeGraph :: Ix i => Graph i a -> Graph i a
transposeGraph g = mkGraphWith bAdj (bounds g) [(u, v, w) | u <- indices g, (v, w) <- g ! u]

-- | グラフの出次数を返す
-- >>> outdegree (mkGraphWith @_ @(Int, Int) fAdj (0,2) [(0,1),(1,2)])
-- array (0,2) [(0,1),(1,1),(2,0)]
outdegree :: Ix i => Graph i a -> UArray i Int
outdegree g = array (bounds g) [(v, length outs) | (v, outs) <- assocs g]

-- | グラフの入次数を返す
-- >>> indegree (mkGraphWith @_ @(Int, Int) fAdj (0,2) [(0,1),(1,2)])
-- array (0,2) [(0,0),(1,1),(2,1)]
indegree :: Ix i => Graph i a -> UArray i Int
indegree g = accumArray (+) 0 (bounds g) [(v,1) | outs <- elems g, (v,_) <- outs]

-- | 隣接する頂点を返す
adj :: Ix i => Graph i a -> i -> [i]
adj g = map fst . (g!)

-- | 隣接する頂点と辺の重みのペアを返す
adjW :: Ix i => Graph i a -> i -> [(i, a)]
adjW = (!)


-- アルゴリズム

-- | DFSにより、DAGをトポロジカルソートして返す
-- ex. onGraph: let dist = topologicalSort (adj graph) (bounds graph)
topologicalSort :: forall i a. Ix i => Graph i a -> [i]
topologicalSort graph = runST $ do
    let b = bounds graph
        nextStatus = adj graph

    seen <- newUArray b False
    order <- newRef []

    for_ (range b) $ \start -> flip fix start $ \loop v -> do
        t <- readArray seen v
        if t then return () else do
            writeArray seen v True
            for_ (nextStatus v) loop
            modifyRef order (v:)

    readRef order

-- | Kahnのアルゴリズムにより、DAGをトポロジカルソートして返す
-- グラフに閉路が存在する場合、空のリストを返す
topoSortKahn :: Graph Int a -> [Int]
topoSortKahn graph = runST $ do
    let b = bounds graph
        succs = adj graph

    indeg <- thaw (indegree graph) :: ST s (STUArray s Int Int)
    order <- newRef []
    count <- newRef (0::Int)

    start <- map fst . filter ((==0) . snd) <$> getAssocs indeg

    flip fix (fromListQ start) $ \loop queue -> do
        case viewQ queue of
            EmptyQ -> return ()
            (v :@ rest) -> do

                modifyRef order (v:)
                modifyRef' count (1+)

                let step q cand = do
                        r <- readArray indeg cand
                        if r == 0 then return q else do
                            writeArray indeg cand (r-1)
                            return $ if r == 1 then q .> cand else q

                foldlM step rest (succs v) >>= loop

    result <- readRef order
    reslen <- readRef count
    return $ if rangeSize b == reslen then reverse result else []

-- | 無向グラフの連結成分のリストを返す
-- 各連結成分は訪問順の逆順に並んでいる
connectedComponents :: forall i a. Ix i => Graph i a -> [[i]]
connectedComponents graph = filter (not . null) $ runST $ do
    let b = bounds graph
        nextStatus = adj graph

    seen <- newUArray b False

    for (range b) $ \s -> do
        ref <- newRef []

        flip fix s $ \loop v -> do
            t <- readArray seen v
            if t then return () else do
                writeArray seen v True
                modifyRef ref (v:)
                for_ (nextStatus v) loop

        readRef ref

-- | 頂点に注目したオイラーツアーを返す
eulerTour :: forall i a. Ix i => Graph i a -> i -> [i]
eulerTour graph start = runST $ do
    let b = bounds graph
        nextStatus = adj graph

    seen <- newUArray b False
    ref <- newRef []

    let f u = do
            r <- readRef ref
            when (null r || head r /= u) (modifyRef ref (u:))

    flip fix start $ \loop v -> do
        t <- readArray seen v
        if t then return () else do
            writeArray seen v True
            for_ (nextStatus v) $ \nv -> do
                f v
                loop nv
                f v

    reverse <$> readRef ref

-- | 有向グラフに1つ以上のサイクルが含まれているか判定する
cycleDetection :: forall i a. Ix i => Graph i a -> Bool
cycleDetection graph = runST $ do
    let b = bounds graph
        nextStatus = adj graph

    seen <- newUArray b False
    finished <- newUArray b False
    isCycle <- newRef False

    for_ (range b) $ \s -> flip fix s $ \loop v -> do
        t <- readArray seen v
        if t then return () else do
            writeArray seen v True
            for_ (nextStatus v) $ \nv -> do

                nt <- readArray seen nv
                nf <- readArray finished nv
                when (nt && not nf) $ writeRef isCycle True

                unless nf $ loop nv

            writeArray finished v True

    readRef isCycle
