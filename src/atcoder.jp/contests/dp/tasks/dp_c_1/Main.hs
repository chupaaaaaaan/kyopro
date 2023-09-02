{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import qualified Data.Vector.Unboxed   as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  abcs <- readLnAsUVecWith3Tuple unconsInt n

  print $ solve n abcs


solve :: Int -> VU.Vector (Int,Int,Int) -> Int
solve n abcs = go 1 $ abcs VU.! 0
  where go :: Int -> (Int,Int,Int) -> Int
        go m (a,b,c)
          | m == n = maximum [a,b,c]
          | otherwise = let (a',b',c') = abcs VU.! m
                        in go (m+1) ((a' + max b c),(b' + max c a),(c' + max a b))


unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

-- for unboxed vector
readLnAsUVecWith :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector a)
readLnAsUVecWith !st !n = VU.unfoldrN n (runStateT st) <$> BS.getLine

-- example: 1D tuple vector
readLnAsUVecWith3Tuple :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector (a,a,a))
readLnAsUVecWith3Tuple !st !n = VU.replicateM n $ (\vec -> (vec VU.! 0, vec VU.! 1, vec VU.! 2)) <$> readLnAsUVecWith st 3
