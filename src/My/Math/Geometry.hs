module My.Math.Geometry where

import qualified Control.Arrow as A
import My.Math.Rational

-- | 2次元平面上の点の平行移動
-- (x,y)を(x+dx,y+dy)に移動する
-- >>> map (transf (1,2)) [(1,1),(3,4),(-2,0)]
-- [(2,3),(4,6),(-1,2)]
transf :: (Int, Int) -> (Int, Int) -> (Int, Int)
transf (dx,dy) = (dx+) A.*** (dy+)

-- | 2次元平面上の点の平行移動
-- (x,y)を(x-dx,y-dy)に移動する
-- >>> map (transr (1,2)) [(1,1),(3,4),(-2,0)]
-- [(0,-1),(2,2),(-3,-2)]
transr :: (Int, Int) -> (Int, Int) -> (Int, Int)
transr (dx,dy) = subtract dx A.*** subtract dy

-- | 2次元平面上の点の回転
-- (0,0)を中心に\pi/2回転する
-- >>> map rot4f [(1,1),(3,4),(-2,0)]
-- [(-1,1),(-4,3),(0,-2)]
--
-- >>> map (rot4f . rot4f . rot4f . rot4f) [(1,1),(3,4),(-2,0)] == [(1,1),(3,4),(-2,0)]
-- True
rot4f :: (Int, Int) -> (Int, Int)
rot4f (x,y) = (-y,x)

-- | 2次元平面上の点の回転
-- (0,0)を中心に-\pi/2回転する
-- >>> map rot4r [(1,1),(3,4),(-2,0)]
-- [(1,-1),(4,-3),(0,2)]
--
-- True
-- >>> map (rot4r . rot4r . rot4r . rot4r) [(1,1),(3,4),(-2,0)] == [(1,1),(3,4),(-2,0)]
-- True
rot4r :: (Int, Int) -> (Int, Int)
rot4r (x,y) = (y,-x)

-- | 2次元平面上の2点の外積を計算する
cross :: (Int, Int) -> (Int, Int) -> Int
cross (x1,y1) (x2,y2) = x1 * y2 - x2 * y1

-- | 2次元平面上の3点が、同一直線状に存在するか判定する
onSameLine :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
onSameLine p0 p1 p2 =
    let q1 = transr p0 p1
        q2 = transr p0 p2
    in cross q1 q2 == 0

-- | 2次元平面上の2点がなす傾きを、(x方向の変化量,y方向の変化量)形式で取得する
-- x方向の変化量は非負である
incl :: (Int, Int) -> (Int, Int) -> Rat
incl (x1,y1) (x2,y2) = (x2 - x1) |% (y2 - y1)

-- | 2次元平面上の2点をm:nに内分する点を取得する
idp :: Int -> Int -> (Int,Int) -> (Int,Int) -> (Rat, Rat)
idp m n (x1,y1) (x2,y2) = ((m+n) |% (n*x1 + m*x2), (m+n) |% (n*y1 + m*y2))

-- | 2次元平面上の2点の中点を取得する
midp :: (Int,Int) -> (Int,Int) -> (Rat, Rat)
midp = idp 1 1
