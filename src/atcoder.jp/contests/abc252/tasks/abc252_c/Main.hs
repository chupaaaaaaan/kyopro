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
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.ByteString.Char8        as BS
import           Data.Char
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
import Data.Ord

main :: IO ()
main = do
  n <- readLn @Int
  vs <- replicateM n $ readLnAsUVecWith unconsChar 10

  cands <- forM ['0'..'9'] $ \i -> do
      let tm = reverse $ sort $ map (fromJust . VU.elemIndex i) vs
          gt = head $ sortOn (Down . length) $ group tm
          qt = if maximum tm == head gt then 0 else  maximum tm - head gt
          cand = maximum tm + 10 * subtract 1 (length gt) - qt

      return cand

  print $ minimum cands


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
readLnAsUVecCWith !st !n = VU.replicateM n $ (VU.! 0) <$> readLnAsUVecWith st 1

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
