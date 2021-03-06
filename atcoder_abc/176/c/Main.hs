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
import qualified Data.ByteString.Char8       as BS
import           Data.Char
import           Data.Int                    (Int64)
import           Data.List
import           Data.Maybe
import           Data.Proxy
import           Data.Sequence               (Seq, (<|), (><), (|>))
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as S
import           Data.STRef
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Numeric

main :: IO ()
main = do
  _ <- getLine
  (a:as) <- readLnAsListWith unconsInt

  print $ solve a as

  return ()


solve :: Int -> [Int] -> Int
solve a as = go as a 0
  where go [] _ c = c
        go (x:xs) h c = if h>x
                        then go xs h (c+h-x)
                        else go xs x c

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
readLnAsUVecWith :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector a)
readLnAsUVecWith !st !n = VU.unfoldrN n (runStateT st) <$> BS.getLine

-- example: 1D tuple vector
readLnAsUVecWith2Tuple :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector (a,a))
readLnAsUVecWith2Tuple !st !n = VU.replicateM n $ (\vec -> (vec VU.! 0, vec VU.! 1)) <$> readLnAsUVecWith st 2


-- 2D Data
-- n: number of lines
-- m: number of values per a line

-- for boxed array
readLnAs2DArrayWith :: IArray a e => StateT BS.ByteString Maybe e -> Int -> Int -> IO (a (Int,Int) e)
readLnAs2DArrayWith !st !n !m = listArray ((1,1),(n,m)) . unfoldr (runStateT st) . BS.concat <$> replicateM n BS.getLine

-- for boxed vector
readLnAs2DVecWith :: StateT BS.ByteString Maybe a -> Int -> Int -> IO (V.Vector (V.Vector a))
readLnAs2DVecWith !st !n !m = V.replicateM n $ V.unfoldrN m (runStateT st) <$> BS.getLine

-- for unboxed vector
readLnAs2DUVecWith :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> Int -> IO (V.Vector (VU.Vector a))
readLnAs2DUVecWith !st !n !m = V.replicateM n $ VU.unfoldrN m (runStateT st) <$> BS.getLine
