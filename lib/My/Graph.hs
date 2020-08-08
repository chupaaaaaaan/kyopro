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
type FlagVec s = VUM.MVector s Bool
type Vertex = Int

-- 解くべき問題により、シグネチャは変更する
treeDfs :: forall a. Vertex -> Graph a -> ()
treeDfs root g = runST $ do
  go (-1) root
  where go :: Vertex -> Vertex -> ST s ()
        go p v = forM_ (g V.! v) $ \(nv, _) -> do
          if p == nv
            then return ()
            else do
            -- 行きがけ順の処理
            go v nv
            -- 帰りがけ順の処理

-- 解くべき問題により、シグネチャは変更する
dfs :: (Num a) => [Vertex] -> Graph a -> ()
dfs vs g = runST $ do
  seen <- VUM.replicate (V.length g) False
  mapM_ (go seen) vs
  where go :: FlagVec s -> Vertex -> ST s ()
        go seen v = forM_ (g V.! v) $ \(nv, _) -> do
          VUM.read seen nv >>= \s -> if not s
            then return ()
            else do VUM.write seen nv True
                    -- 行きがけ順の処理
                    go seen nv
                    -- 帰りがけ順の処理

-- 重みなしグラフ
graph' :: VU.Vector (Vertex, Vertex) -> Int -> Graph ()
graph' = graph . VU.map (\(x,y) -> (x,y,()))

-- 有向グラフの場合は、to側の隣接リスト追加部分をコメントアウト
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
