{-# LANGUAGE BangPatterns #-}
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Array.IArray
import           Data.Array.ST
import           Data.Array.Unboxed    (UArray)
import qualified Data.Array.Unboxed    as U
import           Data.Bits
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.Graph
import           Data.Int              (Int64)
import           Data.List
import           Data.Maybe
import           Data.Proxy
import qualified Data.Set              as S
import           Data.STRef
import           Data.Vector.Unboxed   (Vector)
import qualified Data.Vector.Unboxed   as V
import           Numeric

getAsInt :: IO [Int]
getAsInt =  unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getAsIntLine :: Int -> IO [[Int]]
getAsIntLine n = replicateM n getAsInt

getAsIntArray1Dr :: Int -> IO (UArray Int Int)
getAsIntArray1Dr n = U.listArray (1,n) <$> getAsInt

getAsIntVec1Dr :: IO (Vector Int)
getAsIntVec1Dr = V.fromList <$> getAsInt

getAsIntArray1Dc :: Int -> IO (UArray Int Int)
getAsIntArray1Dc n = U.listArray (1,n) . concat <$> getAsIntLine n

getAsIntVec1Dc :: Int -> IO (Vector Int)
getAsIntVec1Dc n = V.fromList . concat <$> getAsIntLine n

-- n: number of lines
-- m: number of values per a line
getAsIntArray2D :: Int -> Int -> IO (UArray (Int,Int) Int)
getAsIntArray2D n m = U.listArray ((1,1),(n,m)) . concat <$> getAsIntLine n

getAsString :: IO [String]
getAsString = map BS.unpack . BS.words <$> BS.getLine

getAsCharArray1DWithLength :: IO (Int, UArray Int Char)
getAsCharArray1DWithLength = BS.getLine >>= \cs ->
  let n = BS.length cs
      ary = (U.listArray (1,n) $ unfoldr BS.uncons cs) :: UArray Int Char
  in return (n, ary)

-- n: number of lines
-- m: number of chars per a line
getAsCharArray2D :: Int -> Int -> IO (UArray (Int,Int) Char)
getAsCharArray2D n m = U.listArray ((1,1),(n,m)) . unfoldr BS.uncons . BS.concat <$> replicateM n BS.getLine

main :: IO ()
main = do
  [n,x,y] <- getAsInt
  let ary = solve n x y
  forM_ [1..n-1] $ \i -> print $ ary U.! i

solve :: Int -> Int -> Int -> UArray Int Int
solve n x y = runSTUArray $ do
  cu <- newArray (1,n-1) 0
  forM_ [1..n-1] $ \i ->
    forM_ [i+1..n] $ \j -> let a = j - i
                               b = abs (x - i) + 1 + abs (j - y)
                               c = abs (y - i) + 1 + abs (j - x)
                               m = min a $ min b c
                           in readArray cu m >>= \t -> writeArray cu m (t + 1)
  return cu
