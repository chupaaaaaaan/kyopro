{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.State
import           Control.Monad.Trans.State
import           Data.Array.IArray
import           Data.Array.ST
import           Data.Array.Unboxed        (UArray)
import qualified Data.Array.Unboxed        as U
import           Data.Bits
import           Data.ByteString.Char8     (ByteString)
import qualified Data.ByteString.Char8     as BS
import           Data.Char
import           Data.Graph
import           Data.Int                  (Int64)
import           Data.List
import           Data.Maybe
import           Data.Proxy
import           Data.Sequence             (Seq, (<|), (><), (|>))
import qualified Data.Sequence             as Seq
import qualified Data.Set                  as S
import           Data.STRef
import qualified Data.Vector               as V
import qualified Data.Vector.Unboxed       as VU
import           Numeric

main :: IO ()
main = do
  [n,k] <- readLnAsListWith unconsInt
  aary <- getAsIntArray1Dr n
  forM_ [k+1..n] $ \i -> putStrLn $ if solve aary k i then "Yes" else "No"


solve :: UArray Int Int -> Int -> Int -> Bool
solve ary k i = ary ! (i-k) < ary ! i


getAsInt :: IO [Int]
getAsInt =  unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getAsIntLine :: Int -> IO [[Int]]
getAsIntLine n = replicateM n getAsInt

getAsIntArray1Dr :: Int -> IO (UArray Int Int)
getAsIntArray1Dr n = U.listArray (1,n) <$> getAsInt



-- converter
unconsChar :: StateT BS.ByteString Maybe Char
unconsChar = StateT BS.uncons

unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

unconsInteger :: StateT BS.ByteString Maybe Integer
unconsInteger = StateT $ BS.readInteger . BS.dropWhile isSpace

-- for list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith st = unfoldr (runStateT st) <$> BS.getLine

-- for unboxed vector
readLnAsUVecNWith :: VU.Unbox a => Int -> StateT BS.ByteString Maybe a -> IO (VU.Vector a)
readLnAsUVecNWith !n !st = VU.unfoldrN n (runStateT st) <$> BS.getLine

-- for boxed vector
readLnAsVecNWith :: Int -> StateT BS.ByteString Maybe a -> IO (V.Vector a)
readLnAsVecNWith !n !st = V.unfoldrN n (runStateT st) <$> BS.getLine

getAsString :: IO [String]
getAsString = map BS.unpack . BS.words <$> BS.getLine

getAsCharArray1DWithLength :: IO (Int, UArray Int Char)
getAsCharArray1DWithLength = BS.getLine >>= \ !cs ->
  let !n = BS.length cs
      !ary = (U.listArray (1,n) $ unfoldr BS.uncons cs) :: UArray Int Char
  in return (n, ary)

-- n: number of lines
-- m: number of chars per a line
getAsCharArray2D :: Int -> Int -> IO (UArray (Int,Int) Char)
getAsCharArray2D !n !m = U.listArray ((1,1),(n,m)) . unfoldr BS.uncons . BS.concat <$> replicateM n BS.getLine
