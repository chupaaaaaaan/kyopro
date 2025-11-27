{-# OPTIONS_GHC -Wno-type-defaults #-}
module My.Math.Util where

import Data.Bits
import My.Algorithm.BinarySearch


-- | ある数nに含まれる指定した素因数bの冪を求める
-- TODO: 浮動小数点数を経由しない実装を検討する
-- >>> powerOf 3 125
-- 0
--
-- >>> powerOf 5 125
-- 3
powerOf :: Int -> Int -> Int
powerOf b n = bsearch (\i -> n`mod`(b^i) == 0) 0 (ceiling $ logBase (fromIntegral b) $ fromIntegral (n+1))

-- | ある数に含まれる2の冪を求める
powerOf2 :: Int -> Int
powerOf2 = powerOf 2

-- | 拡張されたユークリッドの互除法
-- (u, v, d) = extgcd x y
-- d = gcd x y
-- d == u * x + v * y
-- see https://blog.miz-ar.info/2017/09/euclidean-algorithm/
-- >>> extgcd 12 78
-- (-6,1,6)
extgcd :: Int -> Int -> (Int,Int,Int)
extgcd = go 1 0 0 1
  where go u v u' v' x y
            | y == 0 = (u, v, x)
            | otherwise = let q = x`div`y
                              r = x`mod`y
                          in go u' v' (u-q*u') (v-q*v') y r

-- | 非負整数の平方根の整数部分を返す
-- https://en.wikipedia.org/wiki/Integer_square_root#Using_only_integer_division
-- >>> map (\x -> (x, isqrt x)) [1..16]
-- [(1,1),(2,1),(3,1),(4,2),(5,2),(6,2),(7,2),(8,2),(9,3),(10,3),(11,3),(12,3),(13,3),(14,3),(15,3),(16,4)]
isqrt :: Integral a => a -> a
isqrt n
    | n < 0 = error "Arg must not be negative"
    | n < 2 = n
    | otherwise = go $ n `div` 2
    where go x = let x' = (x + n `div` x) `div` 2
                 in if x' >= x then x else go x'

-- | 非負整数の立方根を返す
-- >>> (cbrt 17) ** 3
-- 17.000000000000004
cbrt :: (Ord a, Fractional a) => a -> a
cbrt n
    | n < 0 = error "Arg must not be negative"
    | otherwise = go $ n / 2
    where go x = let x' = (2*x + n / x^^2) / 3
                 in if x' >= x then x else go x'

-- | 非負整数を逆順p進表示したリストを返す
-- >>> endig 10 392
-- [2,9,3]
endig :: Integral a => a -> a -> [a]
endig !p !x
    | p <= 0 = error "Cardinal must be positive"
    | x < 0 = error "Input must be non-negative"
    | x == 0 = [0]
    | otherwise = map (`mod`p) . takeWhile (>0) $ iterate (`div`p) x
-- L.unfoldr (\ !k -> if k == 0 then Nothing else Just (k`mod`p, k`div`p))

-- | 逆順p進表示リストから非負整数を返す
-- >>> dedig 10 [2,9,3]
-- 392
dedig :: Integral a => a -> [a] -> a
dedig !p !xs
    | p <= 0 = error "Cardinal must be positive"
    | otherwise = sum $ zipWith f (map (p ^) [0..]) xs
    where f x y = if 0 <= y && y < p
                  then x * y
                  else error "Elements of input list must be between [0,p)"
-- foldr' (\x acc -> acc * p + x) 0

-- | 正整数n以上の最小の「2^k型整数」を返す
-- >>> bitceil 3
-- 4
--
-- >>> bitceil 4
-- 4
--
-- >>> bitceil 5
-- 8
--
bitceil :: Int -> Int
bitceil n = go 1
    where go k | k < n = go (k `unsafeShiftL` 1)
               | otherwise = k
