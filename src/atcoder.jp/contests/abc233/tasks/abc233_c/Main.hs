{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List

main :: IO ()
main = do
    [n, x] <- readLnAsListWith unconsInteger
    ass <- forM [1..n] $ \_ -> tail <$> readLnAsListWith unconsInteger

    print $ solve x ass

{- |
>>> solve 40 [[1,8,4],[10,5]]
2
>>> solve 200 [[3,10,10,10],[3,10,10,10],[5,2,2,2,2,2]]
45
>>> solve 1000000000000000000 [[1000000000,1000000000],[1000000000,1000000000],[1000000000,1000000000]]
0
>>> solve 2 [[1,1,1,1],[1,1,1]]

-}
solve :: Integer -> [[Integer]] -> Int
solve x = length . filter (== x) . foldr (\xs ys -> (*) <$> xs <*> ys) [1]

-- converter
unconsInteger :: StateT BS.ByteString Maybe Integer
unconsInteger = StateT $ BS.readInteger . BS.dropWhile isSpace

-- 1D Data
-- list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine
