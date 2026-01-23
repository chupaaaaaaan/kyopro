module My.Math.Prime where

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

-- | 与えられた数を素因数分解する( $O(\sqrt{n})$)
-- >>> factorize' 548634589304
-- [(2,3),(109,1),(3767,1),(167021,1)]
--
-- >>> factorize' 123456789011
-- [(123456789011,1)]
--
-- >>> factorize' 100
-- [(2,2),(5,2)]
--
factorize' :: Int -> [(Prime,Factor)]
factorize' n0
    | n0 <= 1 = []
    | otherwise = go2 n0
    where go2 n = let (f,k) = peel 2 0 n
                  in if f == 0 then go 3 k else (2,f) : go 3 k
          go p n
              | n == 1 = []
              | p * p > n = [(n,1)]
              | otherwise = let (f,k) = peel p 0 n
                            in if f == 0 then go (p+2) k else (p,f) : go (p+2) k

          peel p f n = if n`mod`p == 0
                       then peel p (f+1) (n`div`p)
                       else (f,n)

-- | 予め作っておいたSPF配列を渡して、与えられた数の約数を全列挙する
divisors :: VU.Vector Int -> Int -> [Int]
divisors sv = f2d . factorize sv
    where f2d = L.foldl' (liftA2 (*)) [1] . map (\(p,f) -> map (p ^) [0..f])

-- | 与えられた数の約数を全列挙する( $O(\sqrt{n})$ )
divisors' :: Int -> [Int]
divisors' = f2d . factorize'
    where f2d = L.foldl' (liftA2 (*)) [1] . map (\(p,f) -> map (p ^) [0..f])

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

-- | v_p(N!) : N!中の素因数pの指数を返す
-- >>> vpFact 2 32
-- 31
--
vpFact :: Int -> Int -> Int
vpFact p n
    | p < 2 = error "vpFact: p must be >= 2"
    | n < 0 = error "vpFact: n must be >= 0"
    | otherwise = go 0 n
    where go :: Int -> Int -> Int
          go !a !k | p > k = a
                   | otherwise = let b = k `div` p
                                 in go (a+b) b
