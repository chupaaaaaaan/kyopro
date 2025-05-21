{-# LANGUAGE FunctionalDependencies #-}
module My.Data.Graph where

import Data.Array.IArray

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
-- >>> outdeg (mkGraphWith @_ @(Int, Int) fAdj (0,2) [(0,1),(1,2)])
-- array (0,2) [(0,1),(1,1),(2,0)]
outdeg :: Ix i => Graph i a -> Array i Int
outdeg = fmap length

-- | グラフの入次数を返す
-- >>> indeg (mkGraphWith @_ @(Int, Int) fAdj (0,2) [(0,1),(1,2)])
-- array (0,2) [(0,0),(1,1),(2,1)]
indeg :: Ix i => Graph i a -> Array i Int
indeg graph = accumArray (+) 0 (bounds graph) [(v,1) | outs <- elems graph, (v,_) <- outs]

-- | 隣接する頂点を返す
adj :: Ix i => Graph i a -> i -> [i]
adj g = map fst . (g!)

-- | 隣接する頂点と辺の重みのペアを返す
adjW :: Ix i => Graph i a -> i -> [(i, a)]
adjW = (!)
