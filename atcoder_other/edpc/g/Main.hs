{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.State
import qualified Data.ByteString.Char8       as BS
import           Data.Char
import           Data.List
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  [n,m] <- readLnAsListWith unconsInt
  edges <- readLnAsUVecWith2Tuple unconsInt m
  let g = graph' edges n
      path = dfs [0..n-1] g
  print $ VU.maximum path

type Graph a = V.Vector [(Int, a)]
-- type Graph a = V.Vector (a, [Vertex])
type FlagVec s = VUM.MVector s Bool
type Vertex = Int

-- 解くべき問題により、シグネチャは変更する
dfs :: VU.Unbox a => [Vertex] -> Graph a -> VU.Vector Int
dfs vs g = VU.create $ do
  seen <- VUM.replicate (V.length g) False
  path <- VUM.replicate (V.length g) 0
  mapM_ (go seen path) vs
  return path
  where go :: FlagVec s -> VUM.MVector s Int -> Vertex -> ST s ()
        go seen path v = do
          VUM.write seen v True
          forM_ (g V.! v) $ \(nv, _) -> do
            s <- VUM.read seen nv
            if s
              then do lv <- VUM.read path v
                      lnv <- VUM.read path nv
                      VUM.write path v (max lv (lnv + 1))
              else do go seen path nv
                      lnv <- VUM.read path nv
                      VUM.write path v (lnv + 1)

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
    -- VM.modify g ((f,w):) t
  return g


-- converter
unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

-- 1D Data
-- for list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine

-- for unboxed vector
readLnAsUVecWith :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector a)
readLnAsUVecWith !st !n = VU.unfoldrN n (runStateT st) <$> BS.getLine

-- example: 1D tuple vector
readLnAsUVecWith2Tuple :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector (a,a))
readLnAsUVecWith2Tuple !st !n = VU.replicateM n $ (\vec -> (vec VU.! 0, vec VU.! 1)) <$> readLnAsUVecWith st 2
