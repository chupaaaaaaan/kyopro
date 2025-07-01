module My.Data.Vector where

import Data.Bifunctor
import Data.Ord
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as VG
import My.Algorithm.BinarySearch


{-# INLINE vSort #-}
vSort :: (Ord a, VG.Vector v a) => v a -> v a
vSort = VG.modify VAI.sort

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

{-# INLINE vDiff #-}
vDiff :: (Ord a, VG.Vector v a) => v a -> v a -> v a
vDiff v u = let su = vSortUniq u
                ulen = VG.length su
                p x = let i = bsearch (su`ile`x) (-1) ulen
                      in i == (-1) || i == ulen || su VG.! i /= x
            in VG.filter p v

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
