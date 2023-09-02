{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module My.Grid where

import Control.Monad
import Control.Monad.ST
import qualified Data.Sequence as Seq
import Data.Array.IArray
import Data.Array.ST
import Data.Bifunctor

-- | execute dfs on 2d-grid.
gridDfs ::
    -- | (top-left corner, bottom-right corner)
    ((Int, Int), (Int, Int)) ->
    -- | starting cell
    (Int, Int) ->
    -- | grid in searching
    Array (Int, Int) a ->
    Array (Int, Int) Bool
gridDfs tlbr start grid = runSTArray $ do

    -- visited cell?
    seen <- newArray tlbr False
    go seen start
    return seen

    where
        go :: STArray s (Int, Int) Bool -> (Int, Int) -> ST s ()
        go seen ij  = do
              s <- readArray seen ij
              if s then return () else do
                  writeArray seen ij True
                  let candidates = filter predicate . neighbours $ ij
                  forM_ candidates $ \c -> go seen c

        -- | search condition
        predicate = undefined

        -- | lookup neighbours
        neighbours = arrounds tlbr nei4


-- | execute dfs on 2d-grid.
gridBfs ::
    -- | (top-left corner, bottom-right corner)
    ((Int, Int), (Int, Int)) ->
    -- | starting cell
    (Int, Int) ->
    -- | grid in searching
    Array (Int, Int) a ->
    Array (Int, Int) Int
gridBfs tlbr start grid = runSTArray $ do

    -- distance from the cell of start
    -- '-1' means that the target cell is not visited
    dist <- newArray tlbr (-1)

    when (predicate 0 start) $ do
        writeArray dist start 0
        go dist (Seq.singleton start)

    return dist

    where
        go :: STArray s (Int, Int) Int -> Seq.Seq (Int, Int) -> ST s ()
        go dist seq
            | Seq.null seq = return ()
            | otherwise = do
                  let (ij Seq.:< q') = Seq.viewl seq
                  d <- readArray dist ij
                  candidates <- filterM (fmap (== (-1)) . readArray dist) . filter (predicate (d + 1)) . neighbours $ ij
                  forM_ candidates $ \cand -> writeArray dist cand (d + 1)
                  go dist $ q' Seq.>< Seq.fromList candidates

        -- | search condition
        predicate = undefined

        -- | lookup neighbours
        neighbours = arrounds tlbr nei4



-- | execute 0-1 bfs on 2d-grid.

-- grid01Bfs ::
--     -- | (top-left corner, bottom-right corner)
--     ((Int, Int), (Int, Int)) ->
--     -- | starting cell
--     (Int, Int) ->
--     -- | search condition (distance from starting cell, target cell)
--     (Int -> (Int, Int) -> Bool) ->
--     -- | grid in searching
--     Array (Int, Int) a ->
--     Array (Int, Int) Int
-- grid01Bfs tlbr start predicate grid = runSTArray $ do
--     -- distance from start point
--     distance <- newArray tlbr (-1)
--     costgrid <- newArray tlbr (-1)

--     when (predicate 0 start) $ do
--         writeArray distance start 0
--         writeArray costgrid start 0
--         go distance costgrid (Seq.singleton start)

--     return distance

--   where
--     go :: STArray s (Int, Int) Int -> STArray s (Int, Int) Int -> Seq.Seq (Int, Int) -> ST s ()
--     go distance costgrid seq
--         | Seq.null seq = return ()
--         | otherwise = do
--             let (idx Seq.:< q') = Seq.viewl seq
--             dist <- readArray distance idx
--             cost <- readArray costgrid idx

--             -- cost == 0
--             cand0 <- filterM (readArray distance >=> (return . (== (-1)))) . filter (predicate (dist + 1)) . neighbour4 tlbr $ idx
--             forM_ cand0 $ \cand -> writeArray distance cand (dist + 1)

--             go distance costgrid $ q' Seq.>< Seq.fromList cand0

-- 0-1 BFS
-- distだけで到達済かを判定できないので、seenも使用する
-- ABC 176 D より
-- grid01Bfs :: (Int,Int) -> (Int,Int) -> (Grid a -> Int -> Bool) -> Grid a -> ()
-- grid01Bfs (h',w') (sh,sw) f g = runST $ do
--   seen <- VUM.replicate (h'*w') False
--   dist <- VUM.replicate (h'*w') (-1)

--   VUM.write dist (ix w' (sh,sw)) 0
--   let deque = Seq.empty Seq.|> ix w' (sh,sw)
--   go seen dist deque

--   where go :: MGrid s Bool -> MGrid s Int -> Seq.Seq Int -> ST s ()
--         go seen dist dq =
--           case dq of
--             Seq.Empty -> return ()
--             _         -> do
--               let (v Seq.:< dq') = Seq.viewl dq
--               s <- VUM.read seen v
--               if s then go seen dist dq' else do
--                 VUM.write seen v True
--                 cost <- VUM.read dist v

--                 candCost0 <- filterM (VUM.read dist >=> return . p0 cost) $ neighbour4 v
--                 forM_ candCost0 $ \nv -> VUM.write dist nv cost

--                 candCost1 <- filterM (VUM.read dist >=> return . p1) $ neighbour25 v
--                 forM_ candCost1 $ \nv -> VUM.write dist nv (cost+1)

--                 let nextDq = foldr (Seq.<|) (foldl' (Seq.|>) dq' candCost1) candCost0
--                 go seen dist nextDq

--         neighbour4 :: Int -> [Int]
--         neighbour4 = filter (f g) . neighbours (h',w') [(x,y) | (x,y) <- [(-1,0),(1,0),(0,-1),(0,1)]]

--         neighbour25 :: Int -> [Int]
--         neighbour25 = filter (f g) . neighbours (h',w') [(x,y) | x <- [-2..2], y <- [-2..2], (x,y) /= (0,0)]

--         p0 :: Int -> Int -> Bool -- コスト0となる条件
--         p0 cost = (||) <$> (==(-1)) <*> (>cost)

--         p1 :: Int -> Bool -- コスト1となる条件
--         p1 = (==(-1))




arrounds ((t, l), (b, r)) ns (i, j) = filter inGrid . map (bimap (i +) (j +)) $ ns
    where inGrid (i, j) = t <= i && i <= b && l <= j && j <= r

data Direction = R | L | U | D deriving (Eq, Ord)
toij :: Direction -> (Int, Int)
toij dir = case dir of R -> (0, 1)
                       L -> (0, -1)
                       U -> (-1, 0)
                       D -> (1, 0)

{- | list neighbour cells
 neighbour cells are below.
 o : target cell
 x : neighbour cells

 #.....#
 .     .
 .  x  .
 . xox .
 .  x  .
 .     .
 #.....#
-}
nei4 = [(p, q)| (p,q) <- map toij [R,L,U,D]]
