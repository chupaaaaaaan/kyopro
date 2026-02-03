{-# LANGUAGE BlockArguments #-}
module My.Conv where

import Control.Monad
import Control.Monad.State.Strict
import Data.Array.Unboxed
import Data.Bool
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.List as L
import qualified Data.Vector.Generic as VG

-- | Converter
type Conv = StateT BS.ByteString Maybe

-- | 入力から1文字読みこむ。空白は無視する。
ucChar :: Conv Char
ucChar = StateT (BS.uncons . BS.dropWhile isSpace)

-- | 空白に到達するまで、入力をIntとして読み込む。
ucInt :: Conv Int
ucInt = StateT (BS.readInt . BS.dropWhile isSpace)

-- | 空白に到達するまで、入力をByteString.Char8として読み込む。
ucBS :: Conv BS.ByteString
ucBS = StateT (\bs -> let bs' = BS.dropWhile isSpace bs
                      in if BS.null bs'
                         then Nothing
                         else Just $ BS.break isSpace bs')

int2 :: Conv (Int, Int)
int2 = (,) <$> ucInt <*> ucInt

int3 :: Conv (Int, Int, Int)
int3 = (,,) <$> ucInt <*> ucInt <*> ucInt

int4 :: Conv (Int, Int, Int, Int)
int4 = (,,,) <$> ucInt <*> ucInt <*> ucInt <*> ucInt

int5 :: Conv (Int, Int, Int, Int, Int)
int5 = (,,,,) <$> ucInt <*> ucInt <*> ucInt <*> ucInt <*> ucInt

listN :: Int -> Conv a -> Conv [a]
listN !n !st = replicateM n st

int2list :: Int -> Conv [(Int, Int)]
int2list !n = listN n int2

int3list :: Int -> Conv [(Int, Int, Int)]
int3list !n = listN n int3

int4list :: Int -> Conv [(Int, Int, Int, Int)]
int4list !n = listN n int4

int5list :: Int -> Conv [(Int, Int, Int, Int, Int)]
int5list !n = listN n int5

-- | read a line and convert to Vector
vector0 :: (VG.Vector v a) => Int -> Conv a -> Conv (v a)
vector0 !n !st = VG.replicateM n st

vector1 :: (VG.Vector v a) => a -> Int -> Conv a -> Conv (v a)
vector1 dummy !n !st = do
    vec <- VG.replicateM n st
    return $ dummy `VG.cons` vec

toVec1 :: (VG.Vector v a) => a -> v a -> v a
toVec1 = VG.cons

-- | Output Utility
printYn :: Bool -> Conv String
printYn = pure . bool "No" "Yes"

printGrid :: IArray a Char => a (Int, Int) Char -> Conv String
printGrid grid = do
    let ((_,s),(_,e)) = bounds grid
        f xs = if null xs then Nothing else Just $ L.splitAt (e-s+1) xs

    pure . unlines . L.unfoldr f . elems $ grid
