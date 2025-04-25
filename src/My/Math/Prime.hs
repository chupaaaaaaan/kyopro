module My.Math.Prime where

import Control.Applicative
import Control.Monad
import qualified Data.List as L
import qualified Data.Vector.Unboxing as VU
import qualified Data.Vector.Unboxing.Mutable as VUM

-- | 添字の数字の最初の素因数（Smallest Prime Factor, SPF）を格納した配列を作る(Eratosthenesの篩)
-- もし添字の数字と格納されている数が等しければ、素数
sieve :: Int -> VU.Vector Int
sieve n
    | n < 1 = VU.empty
    | otherwise = VU.create $ do
          vec <- VUM.replicate (n+1) 0
          VUM.write vec 1 1
          forM_ [2..n] $ \i -> do
              b <- VUM.read vec i
              when (b == 0) $ do
                  VUM.write vec i i
                  when (i <= n `div` i) $ forM_ [i*i,(i+1)*i..n] $ \j -> do
                      c <- VUM.read vec j
                      when (c == 0) $ VUM.write vec j i
          return vec


type Prime = Int
type Factor = Int

-- | 予め作っておいたSPF配列を渡して、素因数分解を行う
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

-- | 素因数分解のリストから、与えられた数の約数を全列挙する
divisors :: [(Prime,Factor)] -> [Int]
divisors = L.foldl' (liftA2 (*)) [1] . map (\(p,f) -> map (p ^) [0..f])

-- | 与えられた数の約数を全列挙する( $O(\sqrt{n})$ )
divisors' :: Int -> [Int]
divisors' n = go 1
    where go f | f * f > n = []
               | f * f == n = [f]
               | otherwise = if n`mod`f == 0
                             then (n`div`f) : f : go (f+1)
                             else go (f+1)

-- | 予め作っておいたSPF配列を渡して、与えられた数が素数かを判定する
isPrime :: VU.Vector Int -> Int -> Bool
isPrime sv n
    | n < 2 = False
    | otherwise = sv VU.! n == n

-- | 与えられた数が素数かを判定する( $O(\sqrt{n})$ )
isPrime' :: Int -> Bool
isPrime' n
    | n < 2 = False
    | otherwise = go 2
    where go f | f * f > n = True
               | otherwise = ((n`mod`f) /= 0) && go (f+1)

-- | 予め作っておいたSPF配列を渡して、n以下の素数のリストを構築する。
primes :: VU.Vector Int -> Int -> [Int]
primes sv n = filter (isPrime sv) [2..n]

-- | 素因数分解のリストから、与えられた数の平方因子を計算する
-- 平方因子は、ある正整数 $n$ に対して $n \times k$ が平方数となるような最小の $k$ のこと
squareFactor :: [(Prime,Factor)] -> Int
squareFactor = product . map fst . filter (odd . snd)
