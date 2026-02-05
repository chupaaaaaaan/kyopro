{-# LANGUAGE RecordWildCards #-}

module My.Data.IntBag where

import Data.Foldable
import qualified Data.IntMap.Strict as IM
import Data.Maybe


data IntBag = IB
    { ibSize         :: !Int
    , ibDistinctSize :: !Int
    , intbag         :: !(IM.IntMap Int)
    }
    deriving (Show)

emptyIB :: IntBag
emptyIB =
    IB { ibSize = 0
       , ibDistinctSize = 0
       , intbag = IM.empty
       }

singletonIB :: Int -> IntBag
singletonIB x =
    IB { ibSize = 1
       , ibDistinctSize = 1
       , intbag = IM.singleton x 1
       }

fromListIB :: [Int] -> IntBag
fromListIB xs =
    let (sz,ds,im) = foldl' f (0,0,IM.empty) xs
    in IB { ibSize = sz
          , ibDistinctSize = ds
          , intbag = im
          }
    where
        f :: (Int, Int, IM.IntMap Int) -> Int -> (Int, Int, IM.IntMap Int)
        f (!sz',!ds',!im') k =
            let (old,newIm) = IM.insertLookupWithKey (const (+)) k 1 im'
                newDs = case old of
                    Nothing -> ds'+1
                    Just _ -> ds'
            in (sz'+1,newDs,newIm)

toAssocListIB :: IntBag -> [(Int,Int)]
toAssocListIB IB{..} = IM.assocs intbag

insertIB :: Int -> IntBag -> IntBag
insertIB = insertMultiIB 1

insertMultiIB :: Int -> Int -> IntBag -> IntBag
insertMultiIB n x ib
    | n <= 0 = ib
    | otherwise = let (old,im) = IM.insertLookupWithKey (const (+)) x n (intbag ib)
                  in IB { ibSize = ibSize ib + n
                        , ibDistinctSize = ibDistinctSize ib + if isNothing old then 1 else 0
                        , intbag = im
                        }

deleteIB :: Int -> IntBag -> IntBag
deleteIB = deleteMultiIB 1

deleteMultiIB :: Int -> Int -> IntBag -> IntBag
deleteMultiIB n x ib
    | n <= 0 = ib
    | otherwise = let (delDistCount, delCount, ib') = IM.alterF f x (intbag ib)
                  in IB { ibSize = ibSize ib - delCount
                        , ibDistinctSize = ibDistinctSize ib - delDistCount
                        , intbag = ib'
                        }
    where f :: Maybe Int -> (Int, Int, Maybe Int)
          f Nothing = (0, 0, Nothing)
          f (Just y) = if y <= n then (1, y, Nothing) else (0, n, Just (y - n))

sizeIB :: IntBag -> Int
sizeIB IB{..} = ibSize

distinctSizeIB :: IntBag -> Int
distinctSizeIB IB{..} = ibDistinctSize

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
    let (big, small) | ibDistinctSize iba >= ibDistinctSize ibb = (intbag iba, intbag ibb)
                     | otherwise = (intbag ibb, intbag iba)
        (overlap,im) = IM.foldlWithKey' f (0,big) small
    in IB { ibSize = ibSize iba + ibSize ibb
          , ibDistinctSize = ibDistinctSize iba + ibDistinctSize ibb - overlap
          , intbag = im
          }
    where
        f :: (Int, IM.IntMap Int) -> Int -> Int -> (Int, IM.IntMap Int)
        f (!ov',!im') k v =
            let (old,newIm) = IM.insertLookupWithKey (const (+)) k v im'
                newOv = case old of
                    Nothing -> ov'
                    Just _ -> ov' + 1
            in (newOv,newIm)

unionsIB :: Foldable f => f IntBag -> IntBag
unionsIB = foldl' unionIB emptyIB

lookupMinIB :: IntBag -> Maybe (Int, Int)
lookupMinIB IB{..} = IM.lookupMin intbag

lookupMaxIB :: IntBag -> Maybe (Int, Int)
lookupMaxIB IB{..} = IM.lookupMax intbag
