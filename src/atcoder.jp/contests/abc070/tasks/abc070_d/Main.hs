{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
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
import           Debug.Trace
import           Numeric

main :: IO ()
main = do
  n <- readLn
  abcs <- readLnAsUVecWith3Tuple unconsInt (n-1)
  (q,k) <- (\vec -> (vec VU.! 0, vec VU.! 1)) <$> readLnAsUVecWith unconsInt 2
  xys <- readLnAsUVecWith2Tuple unconsInt q
  let graph = generateGraph abcs n
      distvec = treeDfs (k-1) graph
  VU.forM_ xys $ \(x,y) -> do
    print $ distvec VU.! (x-1) + distvec VU.! (y-1)


type Graph a = V.Vector [(Int, a)]

treeDfs :: forall a. (Num a, VUM.Unbox a) => Int -> Graph a -> VU.Vector a
treeDfs r graph = VU.create $ do
  distance <- VUM.replicate (V.length graph) (0::a)
  go distance (-1) r
  return distance
  where go :: VUM.MVector s a -> Int -> Int -> ST s ()
        go dist p n = forM_ (graph V.! n) $ \(m, x) -> do
          if p == m
            then return ()
            else do d <- VUM.read dist n
                    VUM.write dist m (d + x)
                    go dist n m

generateGraph :: (Num a, VU.Unbox a) => VU.Vector (Int, Int, a) -> Int -> Graph a
generateGraph vec n = V.create $ do
  graph <- VM.replicate n []
  let l = VU.length vec
  forM_ [0..l-1] $ \i -> do
    let (from,to,w) = vec VU.! i
        f = from - 1
        t = to - 1
    VM.modify graph ((t,w):) f
    VM.modify graph ((f,w):) t
  return graph

readLnAsUVecWith3Tuple :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector (a,a,a))
readLnAsUVecWith3Tuple !st !n = VU.replicateM n $ (\vec -> (vec VU.! 0, vec VU.! 1, vec VU.! 2)) <$> readLnAsUVecWith st 3



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

-- for boxed array
readLnAsArrayWith :: StateT BS.ByteString Maybe a -> Int -> IO (Array Int a)
readLnAsArrayWith !st !n = listArray (1,n) <$> readLnAsListWith st

-- for unboxed array
readLnAsUArrayInt :: Int -> IO (UArray Int Int)
readLnAsUArrayInt !n = listArray (1,n) <$> readLnAsListWith unconsInt

readLnAsUArrayChar :: Int -> IO (UArray Int Char)
readLnAsUArrayChar !n = listArray (1,n) <$> readLnAsListWith unconsChar

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
readLnAs2DArrayWith :: StateT BS.ByteString Maybe a -> Int -> Int -> IO (Array (Int,Int) a)
readLnAs2DArrayWith !st !n !m = listArray ((1,1),(n,m)) . unfoldr (runStateT st) . BS.concat <$> replicateM n BS.getLine

-- for boxed vector
readLnAs2DVecWith :: StateT BS.ByteString Maybe a -> Int -> Int -> IO (V.Vector (V.Vector a))
readLnAs2DVecWith !st !n !m = V.replicateM n $ V.unfoldrN m (runStateT st) <$> BS.getLine

-- for unboxed vector
readLnAs2DUVecWith :: VU.Unbox a => StateT BS.ByteString Maybe a -> Int -> Int -> IO (V.Vector (VU.Vector a))
readLnAs2DUVecWith !st !n !m = V.replicateM n $ VU.unfoldrN m (runStateT st) <$> BS.getLine
