{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString.Char8        as BS
import           Data.Char
import qualified Data.Vector.Unboxing         as VU
import qualified Data.Vector.Unboxing.Mutable as VUM

main :: IO ()
main = do
  n <- readLn :: IO Int
  avec <- readLnAsUVecWith unconsInt n
  let setwise = VU.foldl1' gcd avec

  putStrLn $ if VU.null $ VU.filter (>1) $ solve avec
             then "pairwise coprime"
             else if setwise == 1 then "setwise coprime" else "not coprime"

solve :: VU.Vector Int -> VU.Vector Int
solve avec = VU.create $ do
  let sv = sieve (10^6+5)
  counts <- VUM.replicate (10^6+5) 0
  VU.forM_ avec $ \a -> do
    let ps = map fst $ factorize sv a
    forM_ ps $ \p -> do
      c <- VUM.read counts p
      VUM.write counts p (c+1)

  return counts

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


unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

readLnAsUVecWith :: VU.Unboxable a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector a)
readLnAsUVecWith !st !n = VU.unfoldrN n (runStateT st) <$> BS.getLine
