{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
import           Control.Monad
import qualified Control.Monad.State.Strict   as SS
import qualified Data.Vector.Unboxing         as VU
import qualified Data.Vector.Unboxing.Mutable as VUM

main :: IO ()
main = do
  n <- readLn @Int

  print $ solve n

solve :: Int -> Int
solve n = flip SS.evalState 0 $ do
  let sv = sieve n
  forM_ [1..n-1] $ \i -> do
    let fs = product $ map (\(_,f) -> f+1) $ factorize sv (n-i)
    SS.modify' (+ fs)
  SS.get

-- solve n = sum $ flip map [1..n-1] $
--   \i -> VU.product $ VU.map (\(_,f) -> f+1) $ primeFactors (n-i)


-- 添字の数字の最初の素因数（Smallest Prime Factor, SPF）を格納した配列を作る(Eratosthenesの篩)
-- もし添字の数字と格納されている数が等しければ、素数
sieve :: Int -> VU.Vector Int
sieve n = VU.create $ do
  vec <- VUM.replicate (n+1) 0
  VUM.write vec 1 1
  forM_ [2..n] $ \i -> do
    b <- VUM.read vec i
    when (b == 0) $ do
      VUM.write vec i i
      forM_ [i*i,(i+1)*i..n] $ \j -> do
        c <- VUM.read vec j
        when (c == 0) $ VUM.write vec j i
  return vec


type Prime = Int
type Factor = Int

-- 予め作っておいたSPF配列を渡して、素因数分解を行う
factorize :: VU.Vector Int -> Int -> [(Prime,Factor)]
factorize sv n = go 1 (sv VU.! n) (n `div` sv VU.! n)
  where go _ 0 _ = []
        go _ 1 _ = []
        go a p m
          | m == 1 = [(p,a)]
          | otherwise = let m' = sv VU.! m
                        in if p == m'
                           then go (a+1) p (m`div`m')
                           else (p, a) : go 1 m' (m`div`m')

