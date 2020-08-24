{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module My.Grid where

import           Control.Monad
import           Control.Monad.ST
import           Data.List
import           Data.Sequence               (Seq ((:<|), (:|>)), ViewL ((:<)),
                                              ViewR ((:>)), viewl, viewr, (<|),
                                              (><), (|>))
import qualified Data.Sequence               as Seq
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM


type Grid a = VU.Vector a
type MGrid s = VUM.MVector s

-- DFS
gridDfs :: (Int,Int) -> (Int,Int) -> (Grid a -> Int -> Bool) -> Grid a -> ()
gridDfs (h',w') (sh,sw) f g = runST $ do
  seen <- VUM.replicate (h'*w') False
  go seen $ ix w' (sh,sw)

  where go :: MGrid s Bool -> Int -> ST s ()
        go seen v = do
          s <- VUM.read seen v
          if s then return () else do
            VUM.write seen v True
            let candidate = neighbour4 v
            forM_ candidate $ \nv -> go seen nv

        neighbour4 :: Int -> [Int]
        neighbour4 = filter (f g) . neighbours (h',w') [(x,y) | (x,y) <- [(-1,0),(1,0),(0,-1),(0,1)]]


-- BFS
gridBfs :: (Int,Int) -> (Int,Int) -> (Grid a -> Int -> Bool) -> Grid a -> ()
gridBfs (h',w') (sh,sw) f g = runST $ do
  dist <- VUM.replicate (h'*w') (-1)

  VUM.write dist (ix w' (sh,sw)) 0
  let queue = Seq.empty |> ix w' (sh,sw)
  go dist queue

  where go :: MGrid s Int -> Seq Int -> ST s ()
        go dist q =
          case q of
            Seq.Empty -> return ()
            _ -> do
              let (v :< q') = viewl q
              d <- VUM.read dist v
              candidate <- filterM (VUM.read dist >=> (return . (==(-1)))) $ neighbour4 v
              forM_ candidate $ \nv -> VUM.write dist nv (d+1)
              go dist $ foldl' (|>) q' candidate

        neighbour4 :: Int -> [Int]
        neighbour4 = filter (f g) . neighbours (h',w') [(x,y) | (x,y) <- [(-1,0),(1,0),(0,-1),(0,1)]]

-- 0-1 BFS
-- distだけで到達済かを判定できないので、seenも使用する
-- ABC 176 D より
grid01Bfs :: (Int,Int) -> (Int,Int) -> (Grid a -> Int -> Bool) -> Grid a -> ()
grid01Bfs (h',w') (sh,sw) f g = runST $ do
  seen <- VUM.replicate (h'*w') False
  dist <- VUM.replicate (h'*w') (-1)

  VUM.write dist (ix w' (sh,sw)) 0
  let deque = Seq.empty |> ix w' (sh,sw)
  go seen dist deque

  where go :: MGrid s Bool -> MGrid s Int -> Seq Int -> ST s ()
        go seen dist dq =
          case dq of
            Seq.Empty -> return ()
            _         -> do
              let (v :< dq') = viewl dq
              s <- VUM.read seen v
              if s then go seen dist dq' else do
                VUM.write seen v True
                cost <- VUM.read dist v

                candCost0 <- filterM (VUM.read dist >=> return . p0 cost) $ neighbour4 v
                forM_ candCost0 $ \nv -> VUM.write dist nv cost

                candCost1 <- filterM (VUM.read dist >=> return . p1) $ neighbour25 v
                forM_ candCost1 $ \nv -> VUM.write dist nv (cost+1)

                let nextDq = foldr (<|) (foldl' (|>) dq' candCost1) candCost0
                go seen dist nextDq

        neighbour4 :: Int -> [Int]
        neighbour4 = filter (f g) . neighbours (h',w') [(x,y) | (x,y) <- [(-1,0),(1,0),(0,-1),(0,1)]]

        neighbour25 :: Int -> [Int]
        neighbour25 = filter (f g) . neighbours (h',w') [(x,y) | x <- [-2..2], y <- [-2..2], (x,y) /= (0,0)]

        p0 :: Int -> Int -> Bool -- コスト0となる条件
        p0 cost = (||) <$> (==(-1)) <*> (>cost)

        p1 :: Int -> Bool -- コスト1となる条件
        p1 = (==(-1))



-- Grid Index
-- i: height (row)
-- j: width (column)
ix :: Int -> (Int,Int) -> Int
ix w (i,j) = i * w + j

xi :: Int -> Int -> (Int,Int)
xi w ij = (ij `div` w, ij `mod` w)

neighbours :: (Int,Int) -> [(Int,Int)] -> Int -> [Int]
neighbours (h,w) nei ij = let (h',w') = xi w ij
                          in map (ix w) $ filter (inGrid (h,w)) $ map (\(x,y) -> (h'+x,w'+y)) nei
  where inGrid :: (Int,Int) -> (Int,Int) -> Bool
        inGrid (h',w') (i,j) = i>=0 && i<h' && j>=0 && j<w'
