module My.Misc where

import Data.Array.Unboxed
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Vector.Algorithms as VA
import qualified Data.Vector.Unboxing as VU

-- | ランレングス符号化
-- >>> encoderl "><<>><<<<>"
-- [('>',1),('<',2),('>',2),('<',4),('>',1)]
encoderl :: Eq a => [a] -> [(a, Int)]
encoderl = map f . L.group
    where f xs = case xs of
              (x:_) -> (x, length xs)
              [] -> error "impossible: Data.List.group produced []"

-- | ランレングス符号化の復元
-- >>> let testString = "><<>><<<<>"
-- >>> decoderl (encoderl testString) == testString
-- True
decoderl :: [(a, Int)] -> [a]
decoderl = concatMap $ uncurry $ flip replicate

freq :: Ord a => [a] -> M.Map a Int
freq = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty

hist :: (Int, Int) -> [Int] -> UArray Int Int
hist bnds = accumArray @UArray (+) 0 bnds . map (,1)

freqI :: [Int] -> IM.IntMap Int
freqI = foldl' (\m k -> IM.insertWith (+) k 1 m) IM.empty

chain :: (a -> a -> Bool) -> [a] -> [[a]]
chain _ [] = []
chain _ [a] = [[a]]
chain f (a:b:as)
    | a `f` b = case chain f (b:as) of
                    [] -> notComeHere
                    (k:ks) -> (a:k) : ks
    | otherwise = [a] : chain f (b:as)

notComeHere :: a
notComeHere = error "Not come here."

-- | 1次元の座標圧縮
-- 元座標から圧縮座標を得るIntMapと圧縮座標から元座標を得るIntMapを返す
-- >>> import qualified Data.IntMap.Strict as IM
-- >>> let ts = [1,20,300,4000,50000]
-- >>>     (f,r) = compress1d 0 ts
-- >>> f IM.! 50000
-- >>> r IM.! 3
-- 4
-- 4000
compress1d :: Int -> [Int] -> (IM.IntMap Int, IM.IntMap Int)
compress1d start coor = let nubbed = VA.nub (VU.fromList coor)
                            compressed = VU.enumFromN start (VU.length nubbed)
                            f = IM.fromList . VU.toList . uncurry VU.zip
                        in (f (nubbed, compressed), f (compressed, nubbed))
