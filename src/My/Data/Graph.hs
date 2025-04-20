module My.Data.Graph where

import Data.Array.IArray

type Graph i = Array i [i]
type WGraph i a = Array i [(i, a)]

-- | 隣接リスト形式の重み付きグラフを生成する
genWGraph, genWDiGraph :: Ix i =>
    (i, i) ->      -- ^ 頂点の範囲
    [(i, i, a)] -> -- ^ 辺のリスト (開始, 終了, 重み)
    WGraph i a

-- | 無向グラフ
genWGraph b = accumArray (flip (:)) [] b . concatMap (\(p,q,w) -> [(p,(q,w)), (q,(p,w))])

-- | 有向グラフ
genWDiGraph b = accumArray (flip (:)) [] b . map (\(p,q,w) -> (p,(q,w)))

-- | 隣接リスト形式の重みなしグラフを生成する
genGraph, genDiGraph :: Ix i =>
    (i, i) ->   -- ^ 頂点の範囲
    [(i, i)] -> -- ^ 辺のリスト (開始, 終了)
    Graph i

-- | 無向グラフ
genGraph b = accumArray (flip (:)) [] b . concatMap (\(p,q)->[(p,q),(q,p)])

-- | 有向グラフ
genDiGraph = accumArray (flip (:)) []
