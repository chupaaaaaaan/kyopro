module My.ExtGcd where

-- | (u, v, d) = extgcd x y
-- d := gcd x y
-- d == u * x + v * y
-- see https://blog.miz-ar.info/2017/09/euclidean-algorithm/

extgcd :: Int -> Int -> (Int,Int,Int)
extgcd = go 1 0 0 1
  where go u v u' v' x y
            | y == 0 = (x, u, v)
            | otherwise = let q = x`div`y
                              r = x`mod`y
                          in go u' v' (u-q*u') (v-q*v') y r
