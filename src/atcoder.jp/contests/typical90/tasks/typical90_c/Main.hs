{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.List as L
import Data.STRef
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

main :: IO ()
main = do
    n <- readLn @Int
    abv <- colv2 (n -1) unconsInt

    let tree = genGraph' abv n

    print $ solve tree

solve :: Graph () -> Int
solve graph =
    let (va, _) = treeDfs 0 graph
        (vb, d) = treeDfs va graph
     in d + 1

treeDfs :: forall a. Vertex -> Graph a -> (Vertex, Int)
treeDfs root g = runST $ do
    maxRef <- newSTRef (root, 0)
    go maxRef 0 (-1) root
    readSTRef maxRef
  where
    go :: STRef s (Vertex, Int) -> Int -> Vertex -> Vertex -> ST s ()
    go ref c p v = forM_ (g V.! v) $ \(nv, _) -> do
        modifySTRef ref (\(p, q) -> if (c + 1) <= q then (p, q) else (nv, c + 1))
        if p == nv
            then do return ()
            else do go ref (c + 1) v nv

type Vertex = Int
type Graph a = Vector [(Vertex, a)]

genGraph :: Vector (Vertex, Vertex, a) -> Int -> Graph a
genGraph vec n = V.create $ do
    g <- VM.replicate n []
    let l = V.length vec
    forM_ [0 .. l -1] $ \i -> do
        let (from, to, w) = vec V.! i
            f = from - 1
            t = to - 1
        VM.modify g ((t, w) :) f
        VM.modify g ((f, w) :) t
    return g

-- グラフ生成（隣接リスト、辺重みなし）
genGraph' :: Vector (Vertex, Vertex) -> Int -> Graph ()
genGraph' = genGraph . V.map (\(x, y) -> (x, y, ()))

-- Input
-- converter
unconsInt :: StateT ByteString Maybe Int
unconsInt = StateT (BS.readInt . BS.dropWhile isSpace)

-- read a linear data as List
rowl :: StateT ByteString Maybe a -> IO [a]
rowl !st = L.unfoldr (runStateT st) <$> BS.getLine

-- read a column Vector of order n whose element is 2-tuple
colv2 :: Int -> StateT ByteString Maybe a -> IO (Vector (a, a))
colv2 !n !st = V.replicateM n $ (\[a, b] -> (a, b)) <$> rowl st
