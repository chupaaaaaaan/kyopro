module My.Test.QuickCheck where

import Test.QuickCheck qualified as QC
import Data.List qualified as L



-- Generators

genPositiveStrictlyIncreasingList :: QC.Gen [Int]
genPositiveStrictlyIncreasingList = do
    start <- QC.getPositive <$> QC.arbitrary
    L.scanl' (+) start <$> genPositiveList

genPositiveList :: QC.Gen [Int]
genPositiveList = fmap QC.getPositive . QC.getNonEmpty <$> QC.arbitrary
