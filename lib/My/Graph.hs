{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module My.Graph where

import           Control.Monad
import           Control.Monad.ST
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

type Graph a = V.Vector [(Int, a)]
-- type Graph a = V.Vector (a, [Vertex])
type MVec = VUM.MVector
type Vertex = Int

-- Tree DFS
treeDfs :: forall a. Vertex -> Graph a -> ()
treeDfs root g = runST $ do
  go (-1) root

  where go :: Vertex -> Vertex -> ST s ()
        go p v = forM_ (g V.! v) $ \(nv, _) ->
          if p == nv
          then do return ()
          else do go v nv

-- DFS
dfs :: (VU.Unbox a) => [Vertex] -> Graph a -> ()
dfs vs g = runST $ do
  seen <- VUM.replicate (V.length g) False
  mapM_ (go seen) vs

  where go :: MVec s Bool -> Vertex -> ST s ()
        go seen v = do
          s <- VUM.read seen v
          if s then return () else do
            VUM.write seen v True
            forM_ (g V.! v) $ \(nv, _) -> go seen nv

-- 重みなしグラフ
graph' :: VU.Vector (Vertex, Vertex) -> Int -> Graph ()
graph' = graph . VU.map (\(x,y) -> (x,y,()))

-- 有向グラフの場合は、to側の隣接リスト追加部分をコメントアウト
-- 頂点に重みがある場合は、abc 138 dなど
graph :: VU.Unbox a => VU.Vector (Vertex, Vertex, a) -> Int -> Graph a
graph vec n = V.create $ do
  g <- VM.replicate n []
  let l = VU.length vec
  forM_ [0..l-1] $ \i -> do
    let (from,to,w) = vec VU.! i
        f = from - 1
        t = to - 1
    VM.modify g ((t,w):) f
    VM.modify g ((f,w):) t
  return g
