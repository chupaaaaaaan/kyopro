{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List
import qualified Data.Vector.Unboxed   as VU

main :: IO ()
main = do
  [n,w] <- readLnAsListWith unconsInt
  wvs <- readLnAsUVecWith2Tuple unconsInt n

  print $ VU.maximum $ solve n w wvs



solve :: Int -> Int -> VU.Vector (Int,Int) -> VU.Vector Int
solve !n !w !wvs = go 0 (VU.replicate (w+1) 0)
  where go !m !wvec
          | m == n = wvec
          | otherwise =
              let (!wt,!vt) = wvs VU.! m
              in go (m+1) $ VU.generate (w+1) $ \i ->
                if i - wt < 0
                then wvec VU.! i
                else max (wvec VU.! i) (vt + wvec VU.! (i-wt))

unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine

-- for unboxed vector
readLnAsUVecWith :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector a)
readLnAsUVecWith !st !n = VU.unfoldrN n (runStateT st) <$> BS.getLine

-- example: 1D tuple vector
readLnAsUVecWith2Tuple :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector (a,a))
readLnAsUVecWith2Tuple !st !n = VU.replicateM n $ (\vec -> (vec VU.! 0, vec VU.! 1)) <$> readLnAsUVecWith st 2
