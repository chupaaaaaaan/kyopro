module My.Data.Vector where

import Data.Foldable
import Data.Ord
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
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


-- | タプルのリストからVectorを生成する
-- タプルは (index, value) の形式で、0-based indexingを仮定する
-- >>> vFromTuples0 @Int 3 0 [(1,2)]
-- [0,2,0]
vFromTuples0 :: VG.Vector v a => Int -> a -> [(Int, a)] -> v a
vFromTuples0 n def tuples = VG.create $ do
    v <- VGM.replicate n def
    for_ tuples $ uncurry (VGM.write v)
    return v

-- | タプルのリストからVectorを生成する
-- タプルは (index, value) の形式で、1-based indexingを仮定する
-- >>> vFromTuples1 @Int 3 0 [(1,2)]
-- [2,0,0]
vFromTuples1 :: VG.Vector v a => Int -> a -> [(Int, a)] -> v a
vFromTuples1 n def tuples = VG.create $ do
    v <- VGM.replicate n def
    for_ tuples $ \(i,x) -> VGM.write v (i-1) x
    return v
