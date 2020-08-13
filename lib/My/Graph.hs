{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module My.Graph where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Array.Unboxed
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

type Grid a = UArray (Int,Int) a
type FlagGrid s = STUArray s (Int,Int) Bool

-- グリッドグラフのDFS
-- 解くべき問題により、シグネチャは変更する
gridDfs :: forall a. (VU.Unbox a) => (Int,Int) -> (Int,Int) -> (Grid a -> (Int,Int) -> Bool) -> Grid a -> ()
gridDfs (h',w') (sh,sw) f g = runST $ do
  seen <- newArray ((1,1),(h',w')) False
  go seen (sh,sw)

  where go :: FlagGrid s -> (Int,Int) -> ST s ()
        go seen (h,w) = do
          writeArray seen (h,w) True
          let nexts = [(h+1,w),(h-1,w),(h,w+1),(h,w-1)]
          forM_ nexts $ \(nh,nw) -> do
            s <- readArray seen (nh,nw)
            if s || nh < 1 || nh > h' || nw < 1 || nw > w' || g `f` (nh,nw)
              then do return ()
              else do go seen (nh,nw)

type Graph a = V.Vector [(Int, a)]
-- type Graph a = V.Vector (a, [Vertex])
type FlagVec s = VUM.MVector s Bool
type DistVec s = VUM.MVector s Int
type Vertex = Int

-- 木のDFS
-- 解くべき問題により、シグネチャは変更する
treeDfs :: forall a. Vertex -> Graph a -> ()
treeDfs root g = runST $ do
  go (-1) root

  where go :: Vertex -> Vertex -> ST s ()
        go p v = forM_ (g V.! v) $ \(nv, _) ->
          if p == nv
          then do return ()
          else do go v nv

-- 普通のグラフのDFS
-- 解くべき問題により、シグネチャは変更する
dfs :: (VU.Unbox a) => [Vertex] -> Graph a -> ()
dfs vs g = runST $ do
  seen <- VUM.replicate (V.length g) False
  mapM_ (go seen) vs

  where go :: FlagVec s -> Vertex -> ST s ()
        go seen v = do
          VUM.write seen v True
          forM_ (g V.! v) $ \(nv, _) -> do
            s <- VUM.read seen nv
            if s
              then do return ()
              else do go seen nv

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
