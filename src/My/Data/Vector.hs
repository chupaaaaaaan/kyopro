module My.Data.Vector where

import Data.Bifunctor
import Data.Ord
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as VG
import My.Algorithm.BinarySearch
import Data.Maybe


{-# INLINE vSort #-}
vSort :: (Ord a, VG.Vector v a) => v a -> v a
vSort = VG.modify $ VAI.sortBy compare

{-# INLINE vSortOn #-}
vSortOn :: (Ord b, VG.Vector v a) => (a -> b) -> v a -> v a
vSortOn f = VG.modify $ VAI.sortBy (comparing f)

{-# INLINE vSortBy #-}
vSortBy :: (Ord a, VG.Vector v a) => VAI.Comparison a -> v a -> v a
vSortBy f = VG.modify $ VAI.sortBy f

{-# INLINE vSortUniq #-}
vSortUniq :: (Ord a, VG.Vector v a) => v a -> v a
vSortUniq = vSortUniqBy compare

{-# INLINE vSortUniqBy #-}
vSortUniqBy :: (Ord a, VG.Vector v a) => VAI.Comparison a -> v a -> v a
vSortUniqBy f v = VG.create $ do
    mv <- VG.thaw v
    VAI.sortUniqBy f mv

-- | 0-based indexingタプルのリストからVectorを生成する
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> vFromTuples0 @VU.Vector @Int 3 0 [(1,2)]
-- [0,2,0]
vFromTuples0 :: VG.Vector v a => Int -> a -> [(Int, a)] -> v a
vFromTuples0 n def = VG.accum (\_ x -> x) (VG.replicate n def)

-- | 1-based indexingタプルのリストからVectorを生成する
-- >>> import qualified Data.Vector.Unboxed as VU
-- >>> vFromTuples1 @VU.Vector @Int 3 0 [(1,2)]
-- [2,0,0]
vFromTuples1 :: VG.Vector v a => Int -> a -> [(Int, a)] -> v a
vFromTuples1 n def = vFromTuples0 n def . map (first (subtract 1))

-- | 昇順にソート済みのVectorから、初めてkey以上/超過/以下/未満の値となるindexを返す
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let vec :: VU.Vector Int = VU.fromList [3,4,5,5,6,7]
-- >>> vLookupGE 5 vec
-- Just 2
--
-- >>> vLookupGT 5 vec
-- Just 4
--
-- >>> vLookupGE 7 vec
-- Just 5
--
-- >>> vLookupGT 7 vec
-- Nothing
--
-- >>> vLookupLE 5 vec
-- Just 3
--
-- >>> vLookupLT 5 vec
-- Just 1
--
-- >>> vLookupLE 3 vec
-- Just 0
--
-- >>> vLookupLT 3 vec
-- Nothing
--
vLookupGE,vLookupGT,vLookupLE,vLookupLT :: (Ord b, VG.Vector v b) => b -> v b -> Maybe Int
vLookupGE key vec = vLookup ige (VG.length vec) (-1) key vec
vLookupGT key vec = vLookup igt (VG.length vec) (-1) key vec
vLookupLE key vec = vLookup ile (-1) (VG.length vec) key vec
vLookupLT key vec = vLookup ilt (-1) (VG.length vec) key vec

-- | 昇順にソート済みのVectorから、keyと等しい値となるindexの範囲を返す
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let vec :: VU.Vector Int = VU.fromList [3,5,5,7]
-- vLookupEQ 3 vec
vLookupEQ :: (Ord b, VG.Vector v b) => b -> v b -> Maybe (Int, Int)
vLookupEQ key vec = let ge = vLookupGE key vec
                        le = vLookupLE key vec
                    in case (ge,le) of
                           (Just b,Just t) -> if b <= t then Just (b,t) else Nothing
                           _anyOthers -> Nothing

vMember :: (Ord b, VG.Vector v b) => b -> v b -> Bool
vMember key vec = isJust $ vLookupEQ key vec


vLookup :: (Ord b, VG.Vector v b) => (v b -> b -> Int -> Bool) -> Int -> Int -> b -> v b -> Maybe Int
vLookup cond ok ng key vec =
    let idx = bsearch (vec `cond` key) ok ng
    in if idx == ok then Nothing else Just (idx, vec VG.! idx)

-- | Vector上の二分探索で使用可能な「超過・未満・以上・以下」の判定条件
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let sorted :: VU.Vector Int = VU.fromList [3,4,5,5,6,7]
-- >>>     l = VU.length sorted
--
-- >>> VU.indexed sorted
-- [(0,3),(1,4),(2,5),(3,5),(4,6),(5,7)]
--
-- >>> bsearch (sorted`igt`5) l 0
-- 4
--
-- >>> bsearch (sorted`ilt`5) 0 l
-- 1
--
-- >>> bsearch (sorted`ige`5) l 0
-- 2
--
-- >>> bsearch (sorted`ile`5) 0 l
-- 3
--
igt,ilt,ige,ile :: (Ord a, VG.Vector v a) => v a -> a -> Int -> Bool
(vec `igt` key) idx = vec VG.! idx > key
(vec `ilt` key) idx = vec VG.! idx < key
(vec `ige` key) idx = vec VG.! idx >= key
(vec `ile` key) idx = vec VG.! idx <= key
