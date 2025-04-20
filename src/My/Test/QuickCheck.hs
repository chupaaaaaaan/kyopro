module My.Test.QuickCheck where

import qualified Data.List as L
import qualified Test.QuickCheck as QC

-- Generators

genPositiveStrictlyIncreasingList :: QC.Gen [Int]
genPositiveStrictlyIncreasingList = do
    start <- QC.getPositive <$> QC.arbitrary
    L.scanl' (+) start <$> genPositiveList

genPositiveList :: QC.Gen [Int]
genPositiveList = fmap QC.getPositive . QC.getNonEmpty <$> QC.arbitrary
