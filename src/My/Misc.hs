module My.Misc where

import Data.Array.Unboxed
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import qualified Data.Map.Strict as M

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

cross :: (a -> b, c -> d) -> (a, c) -> (b, d)
cross (f, g) = pair (f . fst, g . snd)

freq :: Ord a => [a] -> [(a, Int)]
freq = M.toList . L.foldl' (\m k -> M.insertWith (+) k 1 m) M.empty

hist :: (Int, Int) -> [Int] -> UArray Int Int
hist bnds = accumArray @UArray (+) 0 bnds . map (,1)

freqI :: [Int] -> [(Int, Int)]
freqI = IM.toList . L.foldl' (\m k -> IM.insertWith (+) k 1 m) IM.empty

chain :: (a -> a -> Bool) -> [a] -> [[a]]
chain _ [] = []
chain _ [a] = [[a]]
chain f (a:b:as)
    | a `f` b   = let (k:ks) = chain f (b:as)
                  in (a:k) : ks
    | otherwise = [a] : chain f (b:as)
