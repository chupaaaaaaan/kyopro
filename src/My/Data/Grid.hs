module My.Data.Grid where

import Data.Array.IArray
import Data.Bifunctor

genGrid :: (IArray a e, Ix i, Ix j) => ((i, j), (i, j)) -> [[e]] -> a (i, j) e
genGrid b = listArray b . concat

-- | (i, j) から上下左右4つの位置を列挙する
nei4 :: (Ix i, Num i) => (i, i) -> [(i, i)]
nei4 (i, j) = [bimap (i+) (j+) (p,q) | p <- [-1,0,1], q <- [-1,0,1], p/=q]

-- | (i, j) から上下左右斜め8つの位置を列挙する
nei8 :: (Ix i, Num i) => (i, i) -> [(i, i)]
nei8 (i, j) = [bimap (i+) (j+) (p,q) | p <- [-1,0,1], q <- [-1,0,1], (p,q) /= (0,0)]

-- | (i, j) からマンハッタン距離がd以下である位置を列挙する
neiM :: (Ix i, Num i, Enum i) => i -> (i, i) -> [(i, i)]
neiM d (i, j) = [bimap (i+) (j+) (p,q) | p <- [-d..d], q <- [-d..d], abs p + abs q <= d]

-- | 探索候補点を列挙する
nexts :: forall i. Ix i => (i -> Bool) -> (i, i) -> (i -> [i]) -> i -> [i]
nexts isCand bound neighbours = filter isCand . filter (inRange bound) . neighbours
