module My.IO where

import Control.Monad.State.Strict
import Data.ByteString.Char8 qualified as BS
import Data.Vector.Unboxing qualified as VU
import Data.Char
import Data.List qualified as L
import Control.Monad
import Data.Array.IArray
import Data.Array.Unboxed

int1 :: IO Int
int1 = readLn @Int

int2 :: IO (Int, Int)
int2 = to2 <$> list1 ucInt

int3 :: IO (Int, Int, Int)
int3 = to3 <$> list1 ucInt

intlist :: IO [Int]
intlist = list1 ucInt

int2list :: Int -> IO [(Int, Int)]
int2list n = fmap to2 <$> list2 n ucInt

int3list :: Int -> IO [(Int, Int, Int)]
int3list n = fmap to3 <$> list2 n ucInt

charGrid :: Int -> Int -> IO (UArray (Int, Int) Char)
charGrid h w = toGrid ((1,1),(h,w)) <$> list2 h ucChar

-- | Converter
type Conv = StateT BS.ByteString Maybe

ucChar :: Conv Char
ucChar = StateT (BS.uncons . BS.dropWhile isSpace)

ucInt :: Conv Int
ucInt = StateT (BS.readInt . BS.dropWhile isSpace)

ucBS :: Conv BS.ByteString
ucBS = StateT (\bs -> let bs' = BS.dropWhile isSpace bs
                      in if BS.null bs'
                         then Nothing
                         else Just $ BS.break isSpace bs')

-- | read a linear data as List
list1 :: Conv a -> IO [a]
list1 !st = L.unfoldr (runStateT st) <$> BS.getLine

-- | read multi line data as List
list2 :: Int -> Conv a -> IO [[a]]
list2 !n !st = fmap (L.unfoldr (runStateT st)) <$> replicateM n BS.getLine

-- | read a line and convert to Vector
vector1 :: (VU.Unboxable a) => Int -> Conv a -> IO (VU.Vector a)
vector1 n !st = VU.unfoldrN n (runStateT st) <$> BS.getLine

to1 :: [a] -> a
to1 [a] = a
to1 _ = error "invalid length."

to2 :: [a] -> (a,a)
to2 [a,b] = (a,b)
to2 _ = error "invalid length."

to3 :: [a] -> (a,a,a)
to3 [a,b,c] = (a,b,c)
to3 _ = error "invalid length."

to4 :: [a] -> (a,a,a,a)
to4 [a,b,c,d] = (a,b,c,d)
to4 _ = error "invalid length."

