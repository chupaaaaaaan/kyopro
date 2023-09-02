{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.State.Strict
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.List as L

main :: IO ()
main = do
    [n, l] <- rowl unconsInt
    k <- readLn @Int
    as <- rowl unconsInt

    print $ binarySearch (isOk as l k) 0 l


isOk :: [Int] -> Int -> Int -> Int -> Bool
isOk as l k idx = go 0 0 (0 : as <> [l])
  where
    go :: Int -> Int -> [Int] -> Bool
    go _ _ [] = error "not come here"
    go _ k' [_] = k' >= k + 1
    go a k' (b : c : bs) = if (a + (c - b)) >= idx then go 0 (k' + 1) (c : bs) else go (a + (c - b)) k' (c : bs)

binarySearch :: (Int -> Bool) -> Int -> Int -> Int
binarySearch isOk = go
  where
    go ok ng
        | abs (ok - ng) > 1 =
            let mid = (ok + ng) `div` 2
             in if isOk mid
                    then go mid ng
                    else go ok mid
        | otherwise = ok

-- converter
unconsInt :: StateT ByteString Maybe Int
unconsInt = StateT (BS.readInt . BS.dropWhile isSpace)

-- read a linear data as List
rowl :: StateT ByteString Maybe a -> IO [a]
rowl !st = L.unfoldr (runStateT st) <$> BS.getLine
