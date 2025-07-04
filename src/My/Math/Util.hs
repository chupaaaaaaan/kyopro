{-# OPTIONS_GHC -Wno-type-defaults #-}
module My.Math.Util where

import Data.Foldable
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

-- | 非負整数を逆順で2進表示したリストを返す
-- >>> be 187
-- [1,1,0,1,1,1,0,1]
be :: Integral a => a -> [a]
be x
 | x == 0 = [0]
 | x > 0  = map (`mod`2) . takeWhile (>0) . iterate (`div`2) $ x
 | otherwise = error "Arg must not be negative"

-- | 逆順2進表示リストから非負整数を返す
-- >>> de [1,1,0,1,1,1,0,1]
-- 187
de :: forall a. Integral a => [a] -> a
de x = let d = all (\i -> i == 0 || i == 1) x
       in if d
          then foldr' (\ !i !acc -> i + 2 * acc) 0 x
          else error "List must not contain anything other than 0 or 1"
