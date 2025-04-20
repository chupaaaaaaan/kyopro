module My.Data.Vector where

import Data.Ord
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Unboxing as VU
import My.Algorithm.BinarySearch


{-# INLINE vSort #-}
vSort :: (Ord a, VU.Unboxable a) => VU.Vector a -> VU.Vector a
vSort = VU.modify VAI.sort

{-# INLINE vSortOn #-}
vSortOn :: (Ord b, VU.Unboxable a) => (a -> b) -> VU.Vector a -> VU.Vector a
vSortOn f = VU.modify $ VAI.sortBy (comparing f)

{-# INLINE vSortBy #-}
vSortBy :: (Ord a, VU.Unboxable a) => VAI.Comparison a -> VU.Vector a -> VU.Vector a
vSortBy f = VU.modify $ VAI.sortBy f

{-# INLINE vSortUniq #-}
vSortUniq :: (Ord a, VU.Unboxable a) => VU.Vector a -> VU.Vector a
vSortUniq = vSortUniqBy compare

{-# INLINE vSortUniqBy #-}
vSortUniqBy :: (Ord a, VU.Unboxable a) => VAI.Comparison a -> VU.Vector a -> VU.Vector a
vSortUniqBy f v = VU.create $ do
    mv <- VU.thaw v
    VAI.sortUniqBy f mv

{-# INLINE vDiff #-}
vDiff :: (Ord a, VU.Unboxable a) => VU.Vector a -> VU.Vector a -> VU.Vector a
vDiff v u = let su = vSortUniq u
                ulen = VU.length su
                p x = let i = bsearch (su`_iLE`x) (-1) ulen
                      in i == (-1) || i == ulen || su VU.! i /= x
            in VU.filter p v

