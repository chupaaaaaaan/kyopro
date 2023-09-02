{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.State
import           Control.Monad.Trans.State
import           Data.Array.IArray
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Bits
import qualified Data.ByteString.Char8        as BS
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Proxy
import           Data.Sequence                (Seq ((:<|), (:|>)), ViewL ((:<)),
                                               ViewR ((:>)), viewl, viewr, (<|),
                                               (><), (|>))
import qualified Data.Sequence                as Seq
import qualified Data.Set                     as S
import           Data.STRef
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Unboxing         as VU
import qualified Data.Vector.Unboxing.Mutable as VUM
import           Numeric

main :: IO ()
main = do
  [n,w] <- readLnAsListWith unconsInt
  wvs <- readLnAsUVecWith2Tuple unconsInt n

  -- print $ fst $ VU.maximumBy (compare `on` snd) $ VU.zip (VU.enumFromN (0::Int) (v+1)) $ VU.filter (<=w) $ solve n w wvs
  print $ VU.zip (VU.enumFromN (0::Int) (v+1)) $ VU.filter (<=w) $ solve n w wvs

v :: Int
v = 1000

solve :: Int -> Int -> VU.Vector (Int,Int) -> VU.Vector Int
solve !n !w !wvs = go 0 (0 `VU.cons` VU.replicate v (maxBound`div`10))
  where go !m !vvec
          | m == n = vvec
          | otherwise =
              let (!wt,!vt) = wvs VU.! m
              in go (m+1) $ VU.generate (v+1) $ \i ->
                if i - vt < 0
                then vvec VU.! i
                else min (vvec VU.! i) (wt + vvec VU.! (i-vt))


-- converter
unconsChar :: StateT BS.ByteString Maybe Char
unconsChar = StateT BS.uncons

unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

unconsInteger :: StateT BS.ByteString Maybe Integer
unconsInteger = StateT $ BS.readInteger . BS.dropWhile isSpace

-- 1D Data
-- for list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine

-- for array (boxed or unboxed)
readLnAsArrayWith :: IArray a e => StateT BS.ByteString Maybe e -> Int -> IO (a Int e)
readLnAsArrayWith !st !n = listArray (1,n) <$> readLnAsListWith st

-- for boxed vector
readLnAsVecWith :: StateT BS.ByteString Maybe a -> Int -> IO (V.Vector a)
readLnAsVecWith !st !n = V.unfoldrN n (runStateT st) <$> BS.getLine

-- for unboxed vector
readLnAsUVecWith :: VU.Unboxable a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector a)
readLnAsUVecWith !st !n = VU.unfoldrN n (runStateT st) <$> BS.getLine

-- example: 1D tuple vector
readLnAsUVecWith2Tuple :: VU.Unboxable a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector (a,a))
readLnAsUVecWith2Tuple !st !n = VU.replicateM n $ (\vec -> (vec VU.! 0, vec VU.! 1)) <$> readLnAsUVecWith st 2


-- 2D Data
-- n: number of lines
-- m: number of values per a line

-- for boxed vector
readLnAs2DVecWith :: StateT BS.ByteString Maybe a -> Int -> Int -> IO (V.Vector a)
readLnAs2DVecWith !st !n !m = V.unfoldrN (n * m) (runStateT st) . BS.concat <$> replicateM n BS.getLine

readLnAs2DUVecWith :: VU.Unboxable a => StateT BS.ByteString Maybe a -> Int -> Int -> IO (VU.Vector a)
readLnAs2DUVecWith !st !n !m = VU.unfoldrN (n * m) (runStateT st) . BS.concat <$> replicateM n BS.getLine
