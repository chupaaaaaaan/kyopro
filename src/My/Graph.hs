{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module My.Graph where

import Control.Monad
import Control.Monad.ST
import qualified Data.Sequence as Seq
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as VM

-- | execute dfs on tree.
treeDfs :: forall a. Int -> Vector [(Int, a)] -> ()
treeDfs root g = runST $ do
    go (-1) root
  where
    go :: Int -> Int -> ST s ()
    go p v = forM_ (g V.! v) $ \(nv, _) ->
        if p == nv
            then return ()
            else do go v nv

-- | execute dfs on graph.
dfs ::
    -- | list of start vertex
    [Int] ->
    -- | adjacency list
    Vector [(Int, a)] ->
    Vector Bool
dfs vs graph = V.create $ do
    -- visited vertex?
    seen <- VM.replicate (V.length graph) False
    forM_ vs $ go seen

    return seen

  where
    go :: MVector s Bool -> Int -> ST s ()
    go seen v = do
        s <- VM.read seen v
        if s
            then return ()
            else do
                VM.write seen v True
                forM_ (graph V.! v) $ \(nv, _) -> go seen nv

-- | execute bfs on graph.
bfs ::
    -- | list of start vertex
    [Int] ->
    -- | adjacency list
    Vector [(Int, a)] ->
    Vector Int
bfs vs graph = V.create $ do
    -- distance from the vertex of start
    -- '-1' means that the target vertex is not visited
    dist <- VM.replicate (V.length graph) (-1)

    forM_ vs $ flip (VM.write dist) 0
    forM_ vs $ go dist . Seq.singleton

    return dist

  where
    go :: MVector s Int -> Seq.Seq Int -> ST s ()
    go dist seq
        | Seq.null seq = return ()
        | otherwise = do
            let (cv Seq.:< seq') = Seq.viewl seq
            d <- VM.read dist cv
            candidates <- filterM (\(nv, _) -> (fmap (== (-1)) . VM.read dist) nv) $ graph V.! cv
            forM_ candidates $ \(nv, _) -> VM.write dist nv (d + 1)
            go dist $ seq' Seq.>< Seq.fromList (map fst candidates)

-- | execute bfs on graph.
bft ::
    -- | list of start vertex
    [Int] ->
    -- | adjacency list
    Vector [(Int, a)] ->
    Vector Int
bft vs graph = V.create $ do
    -- distance from the vertex of start
    -- '-1' means that the target vertex is not visited
    dist <- VM.replicate (V.length graph) (-1)

    forM_ vs $ flip (VM.write dist) 0
    forM_ vs $ \v -> go dist 0 [v] []

    return dist

  where
    go :: MVector s Int -> Int -> [Int] -> [Int] -> ST s ()
    go dist _ []        []   = return ()
    go dist d []        next = go dist (d+1) next []
    go dist d (cv:rest) next = do
        n <- (\f -> foldM f next (graph V.! cv)) $ \ nvs (nv, _) -> do
            d' <- VM.read dist nv
            if d' /= (-1) then return nvs else do
                VM.write dist nv (d+1)
                return (nv:nvs)
        go dist d rest n

-- | execute bfs on graph.
bfu ::
    -- | list of start vertex
    [Int] ->
    -- | adjacency list
    Vector [(Int, a)] ->
    Vector Int
bfu vs graph = V.create $ do
    -- distance from the vertex of start
    -- '-1' means that the target vertex is not visited
    dist <- VM.replicate (V.length graph) (-1)

    forM_ vs $ flip (VM.write dist) 0
    forM_ vs $ \v -> go dist 0 [v] []

    return dist

  where
    go :: MVector s Int -> Int -> [Int] -> [Int] -> ST s ()
    go dist _ []        []   = return ()
    go dist d []        next = go dist (d+1) next []
    go dist d (cv:rest) next = do
        n <- (\f -> foldM f next (graph V.! cv)) $ \ nvs (nv, _) -> do
            d' <- VM.read dist nv
            if d' /= (-1) then return nvs else do
                VM.write dist nv (d+1)
                return (nv:nvs)
        go dist d rest n


{- | generate graph as adjacency list
 有向グラフの場合は、to側の隣接リスト追加部分をコメントアウトすれば良い
 頂点に重みがある場合は、abc 138 dなど参照
 https://atcoder.jp/contests/abc138/submissions/15808936
-}
genGraph ::
    -- | tuple of edge (from-vertex, to-vertex, weight)
    Vector (Int, Int, a) ->
    -- | number of vertex
    Int ->
    Vector [(Int, a)]
genGraph vec n = V.create $ do
    g <- VM.replicate n []
    V.forM_ vec $ \(from, to, w) -> do
        let f = from - 1
            t = to - 1
        VM.modify g ((t, w) :) f
        VM.modify g ((f, w) :) t
    return g

-- | generate graph as adjacency list without weight
genGraph' ::
    -- | tuple of edge (from-vertex, to-vertex)
    Vector (Int, Int) ->
    -- | number of vertex
    Int ->
    Vector [(Int, ())]
genGraph' = genGraph . V.map (\(x, y) -> (x, y, ()))
