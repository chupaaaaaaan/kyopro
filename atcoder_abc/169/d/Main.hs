module Main where
import Control.Monad
import Data.STRef
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

primeList :: Int -> [Int]
primeList n = [i | i <- [0..n], sieve n VU.! i]

primeFactors :: Int -> [(Int, Int)]
primeFactors n = filter (\x -> snd x /= 0) $ VU.toList $ factors n

sieve :: Int -> VU.Vector Bool
sieve n = VU.create $ do
  vec <- VUM.replicate (n+1) True
  VUM.write vec 0 False
  VUM.write vec 1 False
  forM_ [2..n] $ \i -> do
    b <- VUM.read vec i
    when b $ forM_ [2*i,3*i..n] $ \j ->
      VUM.write vec j False
  return vec

factors :: Int -> VU.Vector (Int,Int)
factors n = VU.create $ do
  let sn = sqrtn n
  vp <- VUM.replicate (sn+1) True
  vf <- VUM.replicate (sn+1) (0,0)
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
      writeSTRef ir (i+1)
      writeSTRef kr m
  i <- readSTRef ir
  k <- readSTRef kr
  when (k /= 1) $ VUM.write vf i (k, 1)
  return vf

-- Double を経由しているので、ちょっと微妙な実装・・・
sqrtn :: Integral a => a -> a
sqrtn n = let m = floor $ sqrt $ fromIntegral n
          in if n >= m^2 then m else m - 1

-- k = p^a * m
ff :: Int -> Int -> (Int, Int)
ff k p = go k 0
  where go :: Int -> Int -> (Int, Int)
        go m a = if m `mod` p == 0
                 then go (m `div` p) (a + 1)
                 else (a, m)

main :: IO ()
main = do
  [m] <- map read . words <$> getLine :: IO [Int]
  let aa = sum . map (solve . snd) . primeFactors $ m
  print aa

solve :: Int -> Int
solve k = go 1
  where go m = case compare (m * (m + 1)) (2 * k) of
                 LT -> go (m + 1)
                 EQ -> m
                 GT -> m - 1
