{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.State
import qualified Data.ByteString.Char8       as BS
import           Data.Char
import           Data.List
import           Data.Sequence               (Seq ((:<|), (:|>)), ViewL ((:<)),
                                              ViewR ((:>)), viewl, viewr, (<|),
                                              (><), (|>))
import qualified Data.Sequence               as Seq
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  [h,w] <- readLnAsListWith unconsInt
  [cx,cy] <- readLnAsListWith unconsInt
  [dx,dy] <- readLnAsListWith unconsInt
  sgrid <- readLnAs2DUVecWith unconsChar h w :: IO (VU.Vector Char)

  let ggrid = grid01Bfs (h,w) (cx-1,cy-1) (\g ij -> g VU.! ij == '.') sgrid

  print $ ggrid VU.! ix w (dx-1,dy-1)

  return ()


type Grid a = VU.Vector a
type MGrid s = VUM.MVector s

grid01Bfs :: (Int,Int) -> (Int,Int) -> (Grid a -> Int -> Bool) -> Grid a -> VU.Vector Int
grid01Bfs (h',w') (sh,sw) f g = VU.create $ do
  seen <- VUM.replicate (h'*w') False
  dist <- VUM.replicate (h'*w') (-1)

  VUM.write dist (ix w' (sh,sw)) 0
  let deque = Seq.empty |> ix w' (sh,sw)
  go dist seen deque

  return dist

  where go :: MGrid s Int -> MGrid s Bool -> Seq Int -> ST s ()
        go dist seen dq =
          case dq of
            Seq.Empty -> return ()
            _         -> do
              let (curr :< dq') = viewl dq
              s <- VUM.read seen curr
              if s then go dist seen dq' else do
                VUM.write seen curr True
                cost <- VUM.read dist curr

                candCost0 <- filterM (VUM.read dist >=> return . ((||) <$> (==(-1)) <*> (>cost))) $ neighbour4 curr
                forM_ candCost0 $ \next -> VUM.write dist next cost

                candCost1 <- filterM (VUM.read dist >=> return . (==(-1))) $ neighbour25 curr
                forM_ candCost1 $ \next -> VUM.write dist next (cost+1)

                let nextDq = foldr (<|) (foldl' (|>) dq' candCost1) candCost0
                go dist seen nextDq

        neighbour4 :: Int -> [Int]
        neighbour4 = filter (f g) . neighbours (h',w') [(x,y) | (x,y) <- [(-1,0),(1,0),(0,-1),(0,1)]]

        neighbour25 :: Int -> [Int]
        neighbour25 = filter (f g) . neighbours (h',w') [(x,y) | x <- [-2..2], y <- [-2..2], (x,y) /= (0,0)]


-- converter
unconsChar :: StateT BS.ByteString Maybe Char
unconsChar = StateT BS.uncons

unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine

-- for unboxed vector
readLnAs2DUVecWith :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> Int -> IO (VU.Vector a)
readLnAs2DUVecWith !st !n !m = VU.unfoldrN (n * m) (runStateT st) . BS.concat <$> replicateM n BS.getLine


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

