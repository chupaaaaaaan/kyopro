module My.Data.Prime ( primeList
                     , primeFactors
                     )where

import Control.Monad
import Data.STRef
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Debug.Trace

-- n以下の素数のリスト
primeList :: Int -> [Int]
primeList n = [i | i <- [0..n], sieve n VU.! i]

-- 添字の数字が素数か否かをBoolで表現した配列を作る
sieve :: Int -> VU.Vector Bool
sieve n = VU.create $ do
  vec <- VUM.replicate (n+1) True
  VUM.write vec 0 False
  VUM.write vec 1 False
  forM_ [2..n] $ \i -> do
    b <- VUM.read vec i
    when b $ forM_ [2*i,3*i..n] $ \j -> VUM.write vec j False
  return vec


type Prime = Int
type Factor = Int

-- nの素因数分解リスト
primeFactors :: Int -> [(Prime, Factor)]
primeFactors n = filter (\x -> snd x /= 0) $ VU.toList $ factorize n

-- nを素因数分解する
factorize :: Int -> VU.Vector (Prime,Factor)
factorize n = VU.create $ do
  let sn = sqrtn n
  vp <- VUM.replicate (sn+1) True
  vf <- VUM.replicate (gg n) (0,0)
  ir <- newSTRef 0
  kr <- newSTRef n
  VUM.write vp 0 False
  VUM.write vp 1 False
  forM_ [2..sn] $ \p -> do
    b <- VUM.read vp p
    i <- readSTRef ir
    k <- readSTRef kr
    when b $ do
      forM_ [2*p,3*p..sn] $ \j -> VUM.write vp j False
      let (a, m) = ff k p
      VUM.write vf i (p, a)
      when (a /= 0) $ do
        writeSTRef ir (i+1)
        writeSTRef kr m
  i <- readSTRef ir
  k <- readSTRef kr
  when (k /= 1) $ VUM.write vf i (k, 1)
  return vf


-- nの平方根を超えない、最大の整数を求める
-- Double を経由しているので、誤差に起因する超過が無いかチェックする
sqrtn :: Int -> Int
sqrtn n = let m = floor $ sqrt $ fromIntegral n
          in if n >= m^2 then m else m - 1

-- k = p^a * m
-- を満たす(a, m)を求める
ff :: Int -> Int -> (Int, Int)
ff k p = go k 0
  where go :: Int -> Int -> (Int, Int)
        go m a = if m `mod` p == 0
                 then go (m `div` p) (a + 1)
                 else (a, m)


-- nのユニークな素因数の個数の（ナイーブな）上界を求める
-- sqrtn n の（非ユニークな）素因数の個数の上限を求め、
-- それに1を加える。（sqrt n より大きな素因数は高々1個しかないため）
gg :: Int -> Int
gg n = let m = ceiling $ logBase 2 $ fromIntegral $ sqrtn n
       in if n <= 2^m then m + 1 else m + 2
