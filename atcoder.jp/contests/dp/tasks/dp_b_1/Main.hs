{-# LANGUAGE BangPatterns #-}
import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString.Char8       as BS
import           Data.Char
import           Data.List
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  [n,k] <- readLnAsListWith unconsInt
  hvec <- readLnAsUVecWith unconsInt n

  print $ VU.last $ solve4 n k hvec

  return ()


-- mutable vector を使うバージョン（貰うDP）
solve1 :: Int -> Int -> VU.Vector Int -> VU.Vector Int
solve1 n k hvec = VU.create $ do
  dp <- VUM.replicate n 0
  forM_ [0..n-1] $ \i -> case i of
    0 -> VUM.write dp 0 0
    _ -> forM [(max (i-k) 0)..(i-1)] (\j -> liftM2 (+) (VUM.read dp j) (return . abs $ hvec VU.! i - hvec VU.! j)) >>=  VUM.write dp i . minimum
  return dp

-- 貰うDPをimmutable vectorで構成するバージョン
-- 構築済DPテーブルから次のテーブルを構築するので、constructNを使用する
-- https://atcoder.jp/contests/dp/submissions/5209611 を元に、微修正
solve2 :: Int -> Int -> VU.Vector Int -> VU.Vector Int
solve2 n k hvec = VU.constructN n $ \dp ->
  if VU.null dp
  then 0
  else let !i = VU.length dp
           !h_i = hvec VU.! i
       in VU.minimum $ VU.map (\(dp_j, h_j) -> dp_j + abs (h_i - h_j)) $ VU.drop (i-k) $ VU.zip dp hvec

-- h:  0 ------- i-k ------ i-1 -- i ---- n-1

--     (drop)
-- dp: 0 ------- i-k ------ i-1
--     |-- drop --|-- k<=i --|

--     (no drop)
-- dp: 0 ------ i-1 --------------------- n-1
--     |-- i<k --|

-- 配るDP
solve3 :: Int -> Int -> VU.Vector Int -> VU.Vector Int
solve3 n k hvec = VU.create $ do
  dp <- VUM.replicate n maxBound
  VUM.write dp 0 0
  forM_ [0..n-1] $ \i -> forM [(i+1)..(min (i+k) n-1)] $ \j -> do
    a <- liftM2 (+) (VUM.read dp i) (return . abs $ hvec VU.! i - hvec VU.! j)
    p <- VUM.read dp j
    VUM.write dp j $ min a p
  return dp

-- 配るDPをimmutable vectorで構成するバージョン
-- 今の状態から新規のDPテーブルを構築すると考えて、generateを使用する
solve4 :: Int -> Int -> VU.Vector Int -> VU.Vector Int
solve4 !n !k !hvec = go 1 (0 `VU.cons` VU.replicate (k-1) maxBound)
  where go :: Int -> VU.Vector Int -> VU.Vector Int
        go !m !dp | m == n = dp
                  | otherwise =
                      let !dplen = min k (n-m)
                          !h' = VU.drop (m-1) hvec
                      in go (m+1) $ VU.generate dplen $ \j ->
                        if j == k-1
                        then (dp VU.! 0 + abs (h' VU.! (j+1) - h' VU.! 0))
                        else (dp VU.! 0 + abs (h' VU.! (j+1) - h' VU.! 0)) `min` (dp VU.! (j+1))

-- h:  0 --- m-1 -- m ---- m+j ---- m+k-2 -- m+k-1 ---- n-1
-- (drop)     0 --- 1 ---- j+1 ----- k-1 ----- k ------ n-m
-- dp0:       0 --- 1 ---- j+1 ----- k-1
-- dp1:             0 ----- j ------ k-2 ---- k-1

-- n = 13
-- k = 9
--         |--- 13 ----|
-- h:      ooooooooooooo
-- m = 1    |-- 9 --|     k
-- -> dp:   ooooooooo
-- m = 2     |-- 9 --|    k
-- -> dp:    ooooooooo
-- m = 3      |-- 9 --|   k
-- -> dp:     ooooooooo
-- m = 4       |-- 9 --|  k==n-m
-- -> dp:      ooooooooo
-- m = 5        |- 8 --|  n-m
-- -> dp:       oooooooo
-- m = 6         |- 7 -|  n-m
-- -> dp:        ooooooo
-- m = 7          | 6 -|  n-m
-- -> dp:         oooooo
-- m = 8           | 5 |  n-m
-- -> dp:          ooooo
-- m = 9            |4 |  n-m
-- -> dp:           oooo
-- m = 10            |3|  n-m
-- -> dp:            ooo
-- m = 11             ||  n-m
-- -> dp:             oo
-- m = 12              1  n-m
-- -> dp:              o

unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine

readLnAsUVecWith :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector a)
readLnAsUVecWith !st !n = VU.unfoldrN n (runStateT st) <$> BS.getLine

