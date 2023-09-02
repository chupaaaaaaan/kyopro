{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List

main :: IO ()
main = do
    n <- readLn @Int
    as <- readLnAsListWith unconsInt

    print $ solve as


{- |
>>> solve [1,3,2]
2

>>> solve [2,1]
1
p
>>> solve [2,3,4,5,6,7,8,9,10,1]
3

>>> solve [1,2,3,4,5,6,7,8,9,10,11,12]
0
-}
solve :: [Int] -> Int
solve as =
    let (xs, ys) = splitBy (\x y -> abs (x - y) == 1) as
     in case (xs, ys) of
            ([x], []) -> 0
            (x : y : _, []) -> if signum (x - y) > 0 then 1 else 0
            _ ->
                let lx = length xs
                    ly = length ys
                 in if signum (last xs - head ys) > 0
                        then min lx (ly + 2)
                        else min (lx + 1) (ly + 1)

{- |
>>> splitBy (<) [4,5,6,7,1]
([4,5,6,7],[1])

>>> splitBy (<) [1,2,3,4,5]
([1,2,3,4,5],[])

>>> splitBy (<) [5,4,3,2,1]
([5],[4,3,2,1])

>>> splitBy (\x y -> abs (x - y) == 1) [1,3,2]
([1],[3,2])

>>> splitBy (\x y -> abs (x - y) == 1) [5,4,3,2,1]
([5,4,3,2,1],[])
-}
splitBy :: (a -> a -> Bool) -> [a] -> ([a], [a])
splitBy _ [] = ([], [])
splitBy _ [x] = ([x], [])
splitBy p xs@(x : y : xs')
    | p x y = let (ys, zs) = splitBy p (y : xs') in (x : ys, zs)
    | otherwise = ([x], y : xs')

-- converter
unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

-- 1D Data
-- list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine
