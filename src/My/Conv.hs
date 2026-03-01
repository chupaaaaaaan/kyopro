{-# LANGUAGE BlockArguments #-}
module My.Conv where

import Control.Monad
import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxing as VU
import qualified Data.Attoparsec.ByteString.Char8 as AP

-- | Converter
type Conv = StateT BS.ByteString Maybe

-- | 入力から1文字読みこむ。空白は無視する。
ucChar :: Conv Char
ucChar = StateT (BS.uncons . BS.dropWhile isSpace)

-- | 空白に到達するまで、入力をIntとして読み込む。
ucInt :: Conv Int
ucInt = StateT (BS.readInt . BS.dropWhile isSpace)

-- | ByteStringからDoubleの読み取りを試行する。
-- 成功した場合は Just (値, 残りのByteString) を返す。
-- 失敗した場合は Nothing を返す。
readDoubleBS :: BS.ByteString -> Maybe (Double, BS.ByteString)
readDoubleBS bs =
  case AP.parse AP.double bs of
    AP.Done rest x -> Just (x, rest)
    _otherwise     -> Nothing

-- | 空白に到達するまで、入力をDoubleとして読み込む。
ucDouble :: Conv Double
ucDouble = StateT (readDoubleBS . BS.dropWhile isSpace)

-- | 空白に到達するまで、入力をByteString.Char8として読み込む。
ucBS :: Conv BS.ByteString
ucBS = StateT (\bs -> let bs' = BS.dropWhile isSpace bs
                      in if BS.null bs'
                         then Nothing
                         else Just $ BS.break isSpace bs')

-- | 改行に到達するまで、入力をByteString.Char8として読み込む。
ucBSLine :: Conv BS.ByteString
ucBSLine = StateT f
    where f bs = do
              let bs' = BS.dropWhile isSpace bs
              case BS.findIndex (\c -> c == '\n' || c == '\r') bs' of
                  Nothing | BS.null bs' -> Nothing
                          | otherwise -> Just (bs', BS.empty)
                  Just i -> let (l,r0) = BS.splitAt i bs'
                            in case BS.uncons r0 of
                      Nothing -> Just (l, BS.empty)
                      Just (c1,r1) ->
                          let r2 = case BS.uncons r1 of
                                  Just (c2, rest) | c1 == '\r' && c2 == '\n' -> rest
                                  _ -> r1
                          in Just (l, r2)

int2 :: Conv (Int, Int)
int2 = (,) <$> ucInt <*> ucInt

int3 :: Conv (Int, Int, Int)
int3 = (,,) <$> ucInt <*> ucInt <*> ucInt

int4 :: Conv (Int, Int, Int, Int)
int4 = (,,,) <$> ucInt <*> ucInt <*> ucInt <*> ucInt

int5 :: Conv (Int, Int, Int, Int, Int)
int5 = (,,,,) <$> ucInt <*> ucInt <*> ucInt <*> ucInt <*> ucInt

intlist :: Int -> Conv [Int]
intlist !n = replicateM n ucInt

int2list :: Int -> Conv [(Int, Int)]
int2list !n = replicateM n int2

int3list :: Int -> Conv [(Int, Int, Int)]
int3list !n = replicateM n int3

int4list :: Int -> Conv [(Int, Int, Int, Int)]
int4list !n = replicateM n int4

int5list :: Int -> Conv [(Int, Int, Int, Int, Int)]
int5list !n = replicateM n int5

-- | read a line and convert to Vector
vector0 :: VG.Vector v a => Int -> Conv a -> Conv (v a)
vector0 !n !st = VG.replicateM n st

vector1 :: VG.Vector v a => a -> Int -> Conv a -> Conv (v a)
vector1 dummy !n !st = do
    vec <- VG.replicateM n st
    return $ dummy `VG.cons` vec

toVec1 :: (VG.Vector v a) => a -> v a -> v a
toVec1 = VG.cons

intvec :: Int -> Conv (VU.Vector Int)
intvec !n = vector0 n ucInt

int2vec :: Int -> Conv (VU.Vector (Int, Int))
int2vec !n = vector0 n int2

int3vec :: Int -> Conv (VU.Vector (Int, Int, Int))
int3vec !n = vector0 n int3

int4vec :: Int -> Conv (VU.Vector (Int, Int, Int, Int))
int4vec !n = vector0 n int4

int5vec :: Int -> Conv (VU.Vector (Int, Int, Int, Int, Int))
int5vec !n = vector0 n int5
