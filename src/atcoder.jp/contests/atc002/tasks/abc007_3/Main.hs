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
import           Data.Sequence               (Seq ((:<|), (:|>)), ViewL ((:<)),
                                              ViewR ((:>)), viewl, viewr, (<|),
                                              (><), (|>))
import qualified Data.Sequence               as Seq
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  [r,c] <- readLnAsListWith unconsInt
  [sx,sy] <- readLnAsListWith unconsInt
  [gx,gy] <- readLnAsListWith unconsInt
  cgrid <- readLnAs2DUVecWith unconsChar r c

  let ggrid = gridBfs (r,c) (sx-1,sy-1) (\g ij -> g VU.! ij == '.') cgrid

  print $ ggrid VU.! ix c (gx-1,gy-1)
  return ()


type Grid a = VU.Vector a
type MGrid s = VUM.MVector s

-- BFS
gridBfs :: (Int,Int) -> (Int,Int) -> (Grid a -> Int -> Bool) -> Grid a -> VU.Vector Int
gridBfs (h',w') (sh,sw) f g = VU.create $ do
  dist <- VUM.replicate (h'*w') (-1)

  VUM.write dist (ix w' (sh,sw)) 0
  let queue = Seq.empty |> ix w' (sh,sw)
  go dist queue

  return dist


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

-- converter
unconsChar :: StateT BS.ByteString Maybe Char
unconsChar = StateT BS.uncons

unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine

readLnAs2DUVecWith :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> Int -> IO (VU.Vector a)
readLnAs2DUVecWith !st !n !m = VU.unfoldrN (n * m) (runStateT st) . BS.concat <$> replicateM n BS.getLine
