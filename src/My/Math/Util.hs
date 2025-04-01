{-# OPTIONS_GHC -Wno-type-defaults #-}
module My.Math.Util where
import My.Algorithm.BinarySearch


-- | ある数nに含まれる指定した素因数bの冪を求める
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
