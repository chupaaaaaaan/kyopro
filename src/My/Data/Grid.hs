module My.Data.Grid where

import Data.Array.IArray
import Data.Bifunctor

genGrid :: (IArray a e, Ix i, Ix j) => ((i, j), (i, j)) -> [[e]] -> a (i, j) e
genGrid b = listArray b . concat

data Direction = L | R | U | D
    deriving (Show, Read, Eq, Bounded, Enum)

allDirections :: [Direction]
allDirections = [minBound..maxBound]

d2c :: Direction -> Char
d2c L = '<'
d2c R = '>'
d2c U = '^'
d2c D = 'v'

d2i :: Direction -> (Int, Int)
d2i L = (0,-1)
d2i R = (0,1)
d2i U = (-1,0)
d2i D = (1,0)

(^+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(i, j) ^+ (u, v) = (i+u, j+v)

-- | (i, j) から上下左右4つの位置を列挙する
-- >>> nei4 (0,0)
-- [(-1,0),(0,-1),(0,1),(1,0)]
nei4 :: (Int, Int) -> [(Int, Int)]
nei4 (i, j) = [(i, j) ^+ ((p+q)`div`2,(p-q)`div`2) | p <- [-1,1], q <- [-1,1]]

-- | (i, j) から上下左右斜め8つの位置を列挙する
-- >>> nei8 (0,0)
-- [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
nei8 :: (Int, Int) -> [(Int, Int)]
nei8 (i, j) = [bimap (i+) (j+) (p,q) | p <- [-1,0,1], q <- [-1,0,1], (p,q) /= (0,0)]

-- | (i, j) からマンハッタン距離がd以下である位置を列挙する
-- >>> neiM 2 (0,0)
-- [(-2,0),(-1,-1),(-1,0),(-1,1),(0,-2),(0,-1),(0,0),(0,1),(0,2),(1,-1),(1,0),(1,1),(2,0)]
neiM :: Int -> (Int, Int) -> [(Int, Int)]
neiM d (i, j) = [bimap (i+) (j+) (p,q) | p <- [-d..d], q <- [-d..d], abs p + abs q <= d]

-- | 探索候補点を列挙する
nexts :: Ix i => (i -> Bool) -> (i, i) -> (i -> [i]) -> i -> [i]
nexts isCand bound neighbours = filter isCand . filter (inRange bound) . neighbours
