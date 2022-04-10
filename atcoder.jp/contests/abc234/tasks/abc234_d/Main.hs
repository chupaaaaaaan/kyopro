{-# LANGUAGE BangPatterns #-}

import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import qualified Data.IntSet as S

main :: IO ()
main = do
    [n, k] <- readLnAsListWith unconsInt
    xs <- readLnAsListWith unconsInt

    putStrLn $ unlines $ map show $ solve k xs

{- |
>>> solve 2 [1,2,3]
[1,2]

>>> solve 5 [3,7,2,5,11,6,1,9,8,10,4]
[2,3,3,5,6,7,7]

-}
solve :: Int -> [Int] -> [Int]
solve k xs =
    let (as, bs) = splitAt k xs
        set = S.fromList . take k $ as
     in S.findMin set : go set bs
  where
    go :: S.IntSet -> [Int] -> [Int]
    go _ [] = []
    go s (y:ys) = let x = S.findMin s
                  in if x >= y
                     then x : go s ys
                     else let s' = S.insert y . S.deleteMin $ s
                          in S.findMin s' : go s' ys

-- converter
unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace


-- 1D Data
-- list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine
