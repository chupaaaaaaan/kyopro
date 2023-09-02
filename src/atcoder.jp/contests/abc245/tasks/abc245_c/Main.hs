{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8        as BS
import           Data.Char
import           Data.List

main :: IO ()
main = do
  [n,k] <- readLnAsListWith unconsInt
  as <- readLnAsListWith unconsInt
  bs <- readLnAsListWith unconsInt

  putStrLn $ if solve k as bs then "Yes" else "No"


solve :: Int -> [Int] -> [Int] -> Bool
solve _ _ [] = error ""
solve _ [] _ = error ""
solve k (a:as) (b:bs) = go (a,b) as bs
    where go :: (Int,Int) -> [Int] -> [Int] -> Bool
          go  _ [] [] = True
          go  _ _ [] = error ""
          go  _ [] _ = error ""
          go (p,q) (x:xs) (y:ys) = let a' = abs (p-x) <= k
                                       b' = abs (p-y) <= k
                                       c' = abs (q-x) <= k
                                       d' = abs (q-y) <= k
                                   in case (a'||c', b'||d') of
                                          (True, True) -> go (x,y) xs ys
                                          (True, False) -> go (x,x) xs ys
                                          (False, True) -> go (y,y) xs ys
                                          (False, False) -> False

-- converter
unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

-- 1D Data
-- list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine
