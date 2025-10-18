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
insertIB = insertMultiIB 1

insertMultiIB :: Int -> Int -> IntBag -> IntBag
insertMultiIB n x ib =
    IB { ibSize = ibSize ib + n
       , intbag = IM.insertWith (+) x n (intbag ib)
       }

deleteIB :: Int -> IntBag -> IntBag
deleteIB = deleteMultiIB 1

deleteMultiIB :: Int -> Int -> IntBag -> IntBag
deleteMultiIB n x ib =
    let (delCount, ib') = IM.alterF f x (intbag ib)
    in IB { ibSize = max 0 $ ibSize ib - delCount
          , intbag = ib'
          }
    where f :: Maybe Int -> (Int, Maybe Int)
          f Nothing = (0, Nothing)
          f (Just y) = if y <= n then (y, Nothing) else (n, Just (y - n))

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
