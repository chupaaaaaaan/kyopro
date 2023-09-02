{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8       as BS
import           Data.Char
import           Data.List
import qualified Data.Vector                 as V
import qualified Data.Vector.Algorithms.Heap as VH

main :: IO ()
main = do
  n <- readLn @Int
  ps <- concatMap show <$> readLnAsListWith unconsInt
  qs <- concatMap show <$> readLnAsListWith unconsInt

  let rs' = V.fromList $ permutations $ concatMap show [1..n]
  rs'' <- V.thaw rs'
  VH.sort rs''
  rs <- V.freeze rs''

  let a = meguruBSearch (condLE rs ps) (-1) (V.length rs)
      b = meguruBSearch (condLE rs qs) (-1) (V.length rs)

  print $ abs $ a - b


meguruBSearch :: (Int -> Bool) -> Int -> Int -> Int
meguruBSearch isOk = go
  where go ok ng | abs (ok - ng) > 1 = let mid = (ok + ng) `div` 2
                                       in if isOk mid
                                          then go mid ng
                                          else go ok mid
                 | otherwise = ok

condLE :: Ord a => V.Vector a -> a -> Int -> Bool
condLE vec key idx = key >= vec V.! idx


-- converter
unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

-- 1D Data
-- for list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine
