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
import           Data.STRef
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  [n,m] <- readLnAsListWith unconsInt
  abpair <- readLnAsUVecWith2Tuple unconsInt m
  let friendGraph = graph' abpair n

  print $ dfs [0..n-1] friendGraph

  return ()


type Graph a = V.Vector [(Int, a)]
-- type Graph a = V.Vector (a, [Vertex])
type MVec = VUM.MVector
type Vertex = Int

  -- DFS
dfs :: (VU.Unbox a) => [Vertex] -> Graph a -> Int
dfs vs g = runST $ do
  seen <- VUM.replicate (V.length g) False
  ref <- newSTRef 0
  counts <- forM vs $ \v -> do
    writeSTRef ref 0
    go seen ref v
    readSTRef ref

  return $ maximum counts

  where go :: MVec s Bool -> STRef s Int -> Vertex -> ST s ()
        go seen ref v  = do
          s <- VUM.read seen v
          if s then return () else do
            VUM.write seen v True
            modifySTRef' ref (+1)
            forM_ (g V.! v) $ \(nv, _) -> go seen ref nv

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



unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine

readLnAsUVecWith :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector a)
readLnAsUVecWith !st !n = VU.unfoldrN n (runStateT st) <$> BS.getLine

readLnAsUVecWith2Tuple :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector (a,a))
readLnAsUVecWith2Tuple !st !n = VU.replicateM n $ (\vec -> (vec VU.! 0, vec VU.! 1)) <$> readLnAsUVecWith st 2
