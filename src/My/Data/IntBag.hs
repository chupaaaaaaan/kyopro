{-# LANGUAGE RecordWildCards #-}

module My.Data.IntBag where

import Data.Foldable
import qualified Data.IntMap.Strict as IM
import Data.Maybe


data IntBag = IB
    { ibSize :: Int
    , intbag :: IM.IntMap Int
    }
    deriving (Show)

emptyIB :: IntBag
emptyIB =
    IB { ibSize = 0
       , intbag = IM.empty
       }

singletonIB :: Int -> IntBag
singletonIB x =
    IB { ibSize = 1
       , intbag = IM.singleton x 1
       }

fromListIB :: [Int] -> IntBag
fromListIB xs =
    IB { ibSize = length xs
       , intbag = foldl' (\im x -> IM.insertWith (+) x 1 im) IM.empty xs
       }

insertIB :: Int -> IntBag -> IntBag
insertIB x ib =
    IB { ibSize = ibSize ib + 1
       , intbag = IM.insertWith (+) x 1 (intbag ib)
       }

deleteIB :: Int -> IntBag -> IntBag
deleteIB x ib =
    IB { ibSize = ibSize ib - 1
       , intbag = delIB (intbag ib) x
       }
    where delIB :: IM.IntMap Int -> Int -> IM.IntMap Int
          delIB im z = case im IM.!? z of
              Nothing -> im
              Just y -> if y <= 1
                  then IM.delete z im
                  else IM.insert z (y-1) im

sizeIB :: IntBag -> Int
sizeIB IB{..} = ibSize

nullIB :: IntBag -> Bool
nullIB IB{..} = ibSize == 0

countIB :: Int -> IntBag -> Int
countIB x IB{..} = fromMaybe 0 (intbag IM.!? x)

lookupLTIB :: Int -> IntBag -> Maybe (Int, Int)
lookupLTIB x IB{..} = IM.lookupLT x intbag

lookupGTIB :: Int -> IntBag -> Maybe (Int, Int)
lookupGTIB x IB{..} = IM.lookupGT x intbag

lookupLEIB :: Int -> IntBag -> Maybe (Int, Int)
lookupLEIB x IB{..} = IM.lookupLE x intbag

lookupGEIB :: Int -> IntBag -> Maybe (Int, Int)
lookupGEIB x IB{..} = IM.lookupGE x intbag

unionIB :: IntBag -> IntBag -> IntBag
unionIB iba ibb =
    IB { ibSize = ibSize iba + ibSize ibb
       , intbag = IM.unionWith (+) (intbag iba) (intbag ibb)
       }

unionsIB :: Foldable f => f IntBag -> IntBag
unionsIB = foldl' unionIB emptyIB

lookupMinIB :: IntBag -> Maybe (Int, Int)
lookupMinIB IB{..} = IM.lookupMin intbag
