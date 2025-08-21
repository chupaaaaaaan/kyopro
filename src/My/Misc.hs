module My.Misc where

import Data.Array.Unboxed
import Data.Containers.ListUtils
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import qualified Data.Map.Strict as M

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) = pair (f . fst, g . snd)

-- | ランレングス符号化
-- >>> encoderl "><<>><<<<>"
-- [('>',1),('<',2),('>',2),('<',4),('>',1)]
encoderl :: Eq a => [a] -> [(a, Int)]
encoderl = map (\xs -> (head xs, length xs)) . L.group

-- | ランレングス符号化の復元
-- >>> let testString = "><<>><<<<>"
-- >>> decoderl (encoderl testString) == testString
-- True
decoderl :: [(a, Int)] -> [a]
decoderl = concatMap $ uncurry $ flip replicate

freq :: Ord a => [a] -> M.Map a Int
freq = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty

hist :: (Int, Int) -> [Int] -> UArray Int Int
hist bnds = accumArray @UArray (+) 0 bnds . map (,1)

freqI :: [Int] -> IM.IntMap Int
freqI = foldl' (\m k -> IM.insertWith (+) k 1 m) IM.empty

chain :: (a -> a -> Bool) -> [a] -> [[a]]
chain _ [] = []
chain _ [a] = [[a]]
chain f (a:b:as)
    | a `f` b   = let (k:ks) = chain f (b:as)
                  in (a:k) : ks
    | otherwise = [a] : chain f (b:as)

notComeHere :: a
notComeHere = error "Not come here."

-- | 1次元の座標圧縮
compress1d :: Int -> [Int] -> IM.IntMap Int
compress1d start = IM.fromList . flip zip [start..] . nubOrd . L.sort

-- | 2次元平面上の点の平行移動
-- (x,y)を(x+dx,y+dy)に移動する
-- >>> map (transf (1,2)) [(1,1),(3,4),(-2,0)]
-- [(2,3),(4,6),(-1,2)]
transf :: (Int, Int) -> (Int, Int) -> (Int, Int)
transf (dx,dy) = cross ((dx+), (dy+))

-- | 2次元平面上の点の平行移動
-- (x,y)を(x-dx,y-dy)に移動する
-- >>> map (transr (1,2)) [(1,1),(3,4),(-2,0)]
-- [(0,-1),(2,2),(-3,-2)]
transr :: (Int, Int) -> (Int, Int) -> (Int, Int)
transr (dx,dy) = cross (subtract dx, subtract dy)

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
-- [(-1,1),(-4,3),(0,-2)]
--
-- True
-- >>> map (rot4r . rot4r . rot4r . rot4r) [(1,1),(3,4),(-2,0)] == [(1,1),(3,4),(-2,0)]
-- True
rot4r :: (Int, Int) -> (Int, Int)
rot4r (x,y) = (-y,x)

-- | 2次元平面上の3点が、同一直線状に存在するか判定する
onSameLine :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
onSameLine p0 p1 p2 =
    let (x1,y1) = transr p0 p1
        (x2,y2) = transr p0 p2
    in x1 * y2 - x2 * y1 == 0

-- | 2次元平面上の2点がなす傾きを、(x方向の変化量,y方向の変化量)形式で取得する
-- x方向の変化量は非負である
incl :: (Int, Int) -> (Int, Int) -> Rat
incl (x1,y1) (x2,y2) = ri (y2 - y1) (x2 - x1)

-- | 2次元平面上の2点をm:nに内分する点を取得する
idp :: Int -> Int -> (Int,Int) -> (Int,Int) -> (Rat, Rat)
idp m n (x1,y1) (x2,y2) = ((n*x1 + m*x2) `ri` (m+n), (n*y1 + m*y2) `ri` (m+n) )

-- | 2次元平面上の2点の中点を取得する
midp :: (Int,Int) -> (Int,Int) -> ((Int, Int), (Int, Int))
midp = idp 1 1


-- | 有理数のTupleによる表示
-- Ratio IntがUnboxed Vectorに載らないため。
type Numerator = Int
type Denominator = Int
type Rat = (Numerator, Denominator)

-- | 有理数をTupleで表示する
-- 利便性のため、分母が0の場合も許容する
ri :: Int -> Int -> Rat
ri num den = ((num * signum den)`div`d, abs den`div`d)
    where d = gcd num den

-- | 分母を取得する
denom :: Rat -> Denominator
denom (_,den) = den

-- | 分子を取得する
numer :: Rat -> Numerator
numer (num,_) = num

-- | 有理数をDouble型の値に変換する
r2f :: Rat -> Double
r2f (num,den)
    | den == 0 = error "Divided by zero"
    | otherwise = fromIntegral num / fromIntegral den
