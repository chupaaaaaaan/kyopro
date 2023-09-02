{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8        as BS
import           Data.Char
import           Data.List
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Heap  as VA
import qualified Data.Vector.Unboxing         as VU

main :: IO ()
main = do
  [_,m] <- readLnAsListWith unconsInt
  xyvec <- readLnAsUVecWith2Tuple unconsInt m
  lvec <- (VU.thaw (VU.map fst xyvec) >>= \v -> VA.sort v >> VU.freeze v)
  rvec <- (VU.thaw (VU.map snd xyvec) >>= \v -> VA.sort v >> VU.freeze v)

  let l = VU.last lvec
      r = VU.head rvec

  print $ max (r - l + 1) 0

  return ()

-- converter
unconsChar :: StateT BS.ByteString Maybe Char
unconsChar = StateT BS.uncons

unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

unconsInteger :: StateT BS.ByteString Maybe Integer
unconsInteger = StateT $ BS.readInteger . BS.dropWhile isSpace

-- 1D Data
-- list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine

-- boxed vector
readLnAsVecWith :: StateT BS.ByteString Maybe a -> Int -> IO (V.Vector a)
readLnAsVecWith !st !n = V.unfoldrN n (runStateT st) <$> BS.getLine

-- unboxed vector
readLnAsUVecWith :: VU.Unboxable a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector a)
readLnAsUVecWith !st !n = VU.unfoldrN n (runStateT st) <$> BS.getLine

-- unboxed vector (input for n-lines)
readLnAsUVecCWith :: VU.Unboxable a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector a)
readLnAsUVecCWith !st !n = VU.replicateM n $ (\vec -> vec VU.! 0) <$> readLnAsUVecWith st 1

-- unboxed 2-tuple vector
readLnAsUVecWith2Tuple :: VU.Unboxable a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector (a,a))
readLnAsUVecWith2Tuple !st !n = VU.replicateM n $ (\vec -> (vec VU.! 0, vec VU.! 1)) <$> readLnAsUVecWith st 2

-- unboxed 3-tuple vector
readLnAsUVecWith3Tuple :: VU.Unboxable a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector (a,a,a))
readLnAsUVecWith3Tuple !st !n = VU.replicateM n $ (\vec -> (vec VU.! 0, vec VU.! 1, vec VU.! 2)) <$> readLnAsUVecWith st 3

-- 2D Data
-- n: number of lines
-- m: number of values per a line

-- boxed vector
readLnAs2DVecWith :: StateT BS.ByteString Maybe a -> Int -> Int -> IO (V.Vector a)
readLnAs2DVecWith !st !n !m = V.unfoldrN (n * m) (runStateT st) . BS.concat <$> replicateM n BS.getLine

-- unboxed vector
readLnAs2DUVecWith :: VU.Unboxable a => StateT BS.ByteString Maybe a -> Int -> Int -> IO (VU.Vector a)
readLnAs2DUVecWith !st !n !m = VU.unfoldrN (n * m) (runStateT st) . BS.concat <$> replicateM n BS.getLine
