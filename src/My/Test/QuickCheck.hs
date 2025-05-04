module My.Test.QuickCheck where

import qualified Data.List as L
import qualified Test.QuickCheck as QC

-- Generators
-- | Positive-Strictly-Monotomically-Increasing (PSMI) List
genPSMIList :: QC.Gen [Int]
genPSMIList = do
    start <- QC.getPositive <$> QC.arbitrary
    L.scanl' (+) start <$> genPositiveList

-- | Positive-Broadly-Monotomically-Increasing (PBMI) List
genPBMIList :: QC.Gen [Int]
genPBMIList = do
    start <- QC.getPositive <$> QC.arbitrary
    L.scanl' (+) start <$> genNonNegativeList

genPositiveList :: QC.Gen [Int]
genPositiveList = fmap QC.getPositive . QC.getNonEmpty <$> QC.arbitrary

genNonNegativeList :: QC.Gen [Int]
genNonNegativeList = fmap QC.getNonNegative . QC.getNonEmpty <$> QC.arbitrary
