{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module My.Data.Bag where

import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.IntMap.Strict as IM

class BagLike b where

    type Key b

    -- 構築
    emptyB :: b

    -- 更新
    -- adjustB は内部プリミティブ。外部では insertMultiB/deleteMultiB を使う想定。
    -- d>0: insert, d<0: delete, d==0: no-op    
    adjustB :: Int -> Key b -> b -> b

    -- クエリ
    lookupB :: Key b -> b -> Maybe Int

    -- サイズ
    sizeB :: b -> Int
    distinctSizeB :: b -> Int

    -- リスト変換
    -- NOTE: 現状は unionB のために toAssocListB をプリミティブ扱いしている。
    --       性能（中間リスト割当）が問題になったら foldlWithKeyB' を導入して置き換える。
    toAssocListB :: b -> [(Key b,Int)]

data Bag a = B
    { bSize         :: !Int
    , bDistinctSize :: !Int
    , bag           :: !(M.Map a Int)
    }
    deriving (Show)

instance Ord a => BagLike (Bag a) where

    type Key (Bag a) = a

    emptyB = B { bSize = 0
               , bDistinctSize = 0
               , bag = M.empty
               }

    lookupB k B{..} = M.lookup k bag

    adjustB !d x bg@B{..}
        | d == 0 = bg
        | otherwise = let (!ddist, !dsize, !mp') = M.alterF step x bag
                      in B { bSize         = bSize + dsize
                           , bDistinctSize = bDistinctSize + ddist
                           , bag           = mp'
                           }
        where
            step :: Maybe Int -> (Int, Int, Maybe Int)
            step Nothing
                | d > 0     = (1, d, Just d)
                | otherwise = (0, 0, Nothing)
            step (Just c) =
                let new = c + d
                in if new <= 0
                   then (-1, -c, Nothing)
                   else (0, d, Just new)

    sizeB B{..} = bSize

    distinctSizeB B{..} = bDistinctSize

    toAssocListB B{..} = M.assocs bag


data IntBag = IB
    { ibSize         :: !Int
    , ibDistinctSize :: !Int
    , intbag         :: !(IM.IntMap Int)
    }
    deriving (Show)

instance BagLike IntBag where

    type Key IntBag = Int

    emptyB = IB { ibSize = 0
                , ibDistinctSize = 0
                , intbag = IM.empty
                }

    lookupB k IB{..} = IM.lookup k intbag

    adjustB !d x bg@IB{..}
        | d == 0 = bg
        | otherwise = let (!ddist, !dsize, !mp') = IM.alterF step x intbag
                      in IB { ibSize         = ibSize + dsize
                            , ibDistinctSize = ibDistinctSize + ddist
                            , intbag         = mp'
                           }
        where
            step :: Maybe Int -> (Int, Int, Maybe Int)
            step Nothing
                | d > 0     = (1, d, Just d)
                | otherwise = (0, 0, Nothing)
            step (Just c) =
                let new = c + d
                in if new <= 0
                   then (-1, -c, Nothing)
                   else (0, d, Just new)

    sizeB IB{..} = ibSize

    distinctSizeB IB{..} = ibDistinctSize

    toAssocListB IB{..} = IM.assocs intbag

fromListB :: BagLike b => [Key b] -> b
fromListB = foldl' (flip insertB) emptyB

singletonB :: BagLike b => Key b -> b
singletonB x = insertB x emptyB

insertB :: BagLike b => Key b -> b -> b
insertB = insertMultiB 1

insertMultiB :: BagLike b => Int -> Key b -> b -> b
insertMultiB n = adjustB (max 0 n)

deleteB :: BagLike b => Key b -> b -> b
deleteB = deleteMultiB 1

deleteMultiB :: BagLike b => Int -> Key b -> b -> b
deleteMultiB n = adjustB (negate (max 0 n))

infixl 9 !?
(!?) :: BagLike b => b -> Key b -> Maybe Int
(!?) = flip lookupB

countB :: BagLike b => Key b -> b -> Int
countB k = fromMaybe 0 . lookupB k

nullB :: BagLike b => b -> Bool
nullB b = sizeB b == 0

unionB :: BagLike b => b -> b -> b
unionB a b =
    let (big,small) | distinctSizeB a >= distinctSizeB b = (a,b)
                    | otherwise = (b,a)
    in foldl' (\acc (k,n) -> insertMultiB n k acc) big (toAssocListB small)

unionsB :: (BagLike b, Foldable f) => f b -> b
unionsB = foldl' unionB emptyB

class BagLike b => OrderedBagLike b where

    lookupLTB  :: Key b -> b -> Maybe (Key b, Int)
    lookupLEB  :: Key b -> b -> Maybe (Key b, Int)
    lookupGTB  :: Key b -> b -> Maybe (Key b, Int)
    lookupGEB  :: Key b -> b -> Maybe (Key b, Int)
    lookupMinB :: b -> Maybe (Key b, Int)
    lookupMaxB :: b -> Maybe (Key b, Int)

instance Ord a => OrderedBagLike (Bag a) where

    lookupLTB x B{..} = M.lookupLT x bag
    lookupGTB x B{..} = M.lookupGT x bag
    lookupLEB x B{..} = M.lookupLE x bag
    lookupGEB x B{..} = M.lookupGE x bag
    lookupMinB B{..} = M.lookupMin bag
    lookupMaxB B{..} = M.lookupMax bag

instance OrderedBagLike IntBag where

    lookupLTB x IB{..} = IM.lookupLT x intbag
    lookupGTB x IB{..} = IM.lookupGT x intbag
    lookupLEB x IB{..} = IM.lookupLE x intbag
    lookupGEB x IB{..} = IM.lookupGE x intbag
    lookupMinB IB{..} = IM.lookupMin intbag
    lookupMaxB IB{..} = IM.lookupMax intbag
