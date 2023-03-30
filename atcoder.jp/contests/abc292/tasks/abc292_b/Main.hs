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
import qualified Data.Map as M


main :: IO ()
main = do
  [n,q] <- readLnAsListWith unconsInt
  xy <- VU.toList <$> readLnAsUVecWith2Tuple unconsInt q

  forM_ (solve xy) putStrLn


solve :: [(Int, Int)] -> [String]
solve cxs = let mm = M.empty
            in go mm cxs
    where go :: M.Map Int Int -> [(Int, Int)] -> [String]
          go m [] = []
          go m ((1,x):cxs') = let s = M.lookup x m
                              in case s of
                                     Nothing -> go (M.insert x 1 m) cxs'
                                     Just _ -> go (M.insert x 2 m) cxs'
          go m ((2,x):cxs') = go (M.insert x 2 m) cxs'
          go m ((3,x):cxs') = let s = M.lookup x m
                              in case s of
                                     Just 2 -> "Yes" : go m cxs'
                                     Just _ -> "No" : go m cxs'
                                     Nothing -> "No" : go m cxs'
          go m ((_,x):cxs') = error "Not come here"
                                     
                                     





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
