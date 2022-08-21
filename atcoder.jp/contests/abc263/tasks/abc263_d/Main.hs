{-# LANGUAGE BangPatterns #-}

import Control.Monad.ST
import Control.Monad.State.Strict
import qualified Data.Array.IArray as IA
import qualified Data.Array.ST as SA
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import Data.STRef
import qualified Data.Vector.Unboxing as VU

main :: IO ()
main = do
    [n, l, r] <- readLnAsListWith unconsInt
    avec <- readLnAsUVecWith unconsInt n
    print $ solve n l r avec

solve :: Int -> Int -> Int -> VU.Vector Int -> Int
solve = solve1

-- DP （最後の値にのみ興味がある場合：明示的な再帰による実装）
solve5 :: Int -> Int -> Int -> VU.Vector Int -> Int
solve5 n l r av = go 0 (l, av VU.! 0, r)
  where
    go :: Int -> (Int, Int, Int) -> Int
    go k (a, b, c)
        | k == n - 1 = minimum [a, b, c]
        | otherwise = go (k + 1) (l + a, av VU.! (k + 1) + minimum [a, b], r + minimum [a, b, c])

-- DP （最後の値にのみ興味がある場合：Stateによる実装）
solve4 :: Int -> Int -> Int -> VU.Vector Int -> Int
solve4 n l r av = minimum [a, b, c]
  where
    (a, b, c) = (`execState` (l, av VU.! 0, r)) $ do
        forM_ [1 .. n - 1] $ \i -> do
            (pl, po, pr) <- get
            put (l + pl, (av VU.! i) + minimum [pl, po], r + minimum [pl, po, pr])

-- DP （最後の値にのみ興味がある場合：STRefによる実装）
solve3 :: Int -> Int -> Int -> VU.Vector Int -> Int
solve3 n l r av = minimum [a, b, c]
  where
      (a, b, c) = runST $ do
          ref <- newSTRef (l, av VU.! 0, r)
          forM_ [1 .. n - 1] $ \i -> do
              (pl, po, pr) <- readSTRef ref
              writeSTRef ref (l + pl, (av VU.! i) + minimum [pl, po], r + minimum [pl, po, pr])

          readSTRef ref

-- DP（DPテーブルを作成する場合：STUArrayによる実装）
solve2 :: Int -> Int -> Int -> VU.Vector Int -> Int
solve2 n l r av = minimum [res IA.! (n -1, 0), res IA.! (n -1, 1), res IA.! (n -1, 2)]
  where
    res = SA.runSTUArray $ do
        dp <- SA.newArray ((0, 0), (n -1, 2)) (maxBound :: Int)
        SA.writeArray dp (0, 0) l
        SA.writeArray dp (0, 1) (av VU.! 0)
        SA.writeArray dp (0, 2) r

        forM_ [1 .. n -1] $ \i -> do
            pl <- SA.readArray dp (i -1, 0)
            po <- SA.readArray dp (i -1, 1)
            pr <- SA.readArray dp (i -1, 2)

            SA.writeArray dp (i, 0) $ l + pl
            SA.writeArray dp (i, 1) $ (av VU.! i) + minimum [pl, po]
            SA.writeArray dp (i, 2) $ r + minimum [pl, po, pr]

        return dp

-- 置換操作による変化のminimumによる累積和
solve1 :: Int -> Int -> Int -> VU.Vector Int -> Int
solve1 n l r av =
    let vsum = VU.sum av
        lmin = VU.scanl1 min $ VU.scanl (\a t -> a + (l - t)) 0 av
        rmin = VU.scanr1 min $ VU.scanr (\t a -> (r - t) + a) 0 av
     in vsum + VU.minimum (VU.zipWith (+) lmin rmin)

-- converter
unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

-- 1D Data
-- list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine

-- unboxed vector
readLnAsUVecWith :: VU.Unboxable a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector a)
readLnAsUVecWith !st !n = VU.unfoldrN n (runStateT st) <$> BS.getLine
