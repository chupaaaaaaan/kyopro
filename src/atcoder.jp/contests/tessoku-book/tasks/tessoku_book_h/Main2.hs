-- {{{ Imports and Language Extensions
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-dodgy-imports #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST
import Control.Monad.State
import Data.Array.Base (IArray (numElements))
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed (UArray)
import Data.Bifunctor
import Data.Bits
import Data.Bool
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char
import Data.Coerce (coerce)
import Data.Foldable
import Data.Function
import qualified Data.HashPSQ as HashPSQ
import qualified Data.Hashable as Hashable
import qualified Data.Heap as Heap
import Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.IntPSQ as IntPSQ
import qualified Data.IntSet as IS
import Data.List
import Data.List.Extra hiding ((!?))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord
import qualified Data.OrdPSQ as OrdPSQ
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (<|), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Traversable
import Data.Tuple
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Debug.Trace (traceShow)

-- }}}

main :: IO ()
main = do
  [h, w] <- getInts
  grid <- getIntGrid ((1, 1), (h, w))
  q <- getInt
  qs <- replicateM q getInts

  let s = fromArrayCS grid

  for_ qs $ \[a, b, c, d] -> do
    print $ rectRangeQuery (s !) (a, b) (c + 1, d + 1)

{-- IO --}

yn :: Bool -> String
yn = bool "No" "Yes"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

getInt :: IO Int
getInt = fst . fromJust . BS.readInt <$> BS.getLine

getInts :: IO [Int]
getInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getIntsVec :: Int -> IO (VU.Vector Int)
getIntsVec n = VU.unfoldrN n (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getTuple :: IO (Int, Int)
getTuple = auto

getTriple :: IO (Int, Int, Int)
getTriple = auto

getWeightedEdge :: IO ((Int, Int), Int)
getWeightedEdge = do
  [u, v, w] <- getInts
  return ((u, v), w)

getIntArray :: Ix i => (i, i) -> IO (UArray i Int)
getIntArray b = listArray @UArray b <$> getInts

getCharArray :: Ix i => (i, i) -> IO (UArray i Char)
getCharArray b = listArray @UArray b <$> getLine

getStringArray :: IO (UArray Int Char)
getStringArray = do
  s <- BS.getLine
  return $ listArray @UArray (1, BS.length s) (BS.unpack s)

getCharGrid :: Ix b => ((Int, b), (Int, b)) -> IO (UArray (Int, b) Char)
getCharGrid b@((s, _), (h, _)) = do
  xs <- replicateM (h + 1 - s) BS.getLine
  return (listArray @UArray b $ BS.unpack $ BS.concat xs)

getIntGrid :: ((Int, Int), (Int, Int)) -> IO (UArray (Int, Int) Int)
getIntGrid b@((s, _), (h, _)) = do
  xs <- replicateM (h + 1 - s) getInts
  return (listArray @UArray b $ concat xs)

printCharGrid :: (IArray a Char, Ix v) => a (v, Int) Char -> IO ()
printCharGrid grid = traverse_ putStrLn $ chunksOf w (elems grid)
  where
    ((_, w1), (_, w2)) = bounds grid
    w = w2 + 1 - w1

printIntGrid :: (Show e, IArray a e, Ix v) => a (v, Int) e -> IO ()
printIntGrid grid = traverse_ (putStrLn . unwords . map show) $ chunksOf w (elems grid)
  where
    ((_, w1), (_, w2)) = bounds grid
    w = w2 + 1 - w1

{-- auto --}
-- via https://github.com/toyboot4e/toy-lib/blob/main/src/ToyLib/IO.hs

-- | Read from a space-delimited `ByteString`.
class ReadBS a where
  {-# INLINE convertBS #-}
  convertBS :: BS.ByteString -> a
  default convertBS :: (Read a) => BS.ByteString -> a
  convertBS = read . BS.unpack

  -- | For use with `U.unfoldrExactN`.
  {-# INLINE readBS #-}
  readBS :: BS.ByteString -> (a, BS.ByteString)
  readBS !bs =
    let (!bs1, !bs2) = BS.break isSpace bs
     in (convertBS bs1, bs2)

  -- | For use with `U.unfoldr`.
  {-# INLINE readMayBS #-}
  readMayBS :: BS.ByteString -> Maybe (a, BS.ByteString)
  readMayBS !bs
    | BS.null bs = Nothing
    | otherwise =
      let (!bs1, !bs2) = BS.break isSpace bs
       in Just (convertBS bs1, bs2)

instance ReadBS Int where
  {-# INLINE convertBS #-}
  convertBS = fst . readBS
  {-# INLINE readBS #-}
  readBS = fromJust . readMayBS
  {-# INLINE readMayBS #-}
  readMayBS = BS.readInt

instance ReadBS Integer where
  {-# INLINE convertBS #-}
  convertBS = fst . readBS
  {-# INLINE readBS #-}
  readBS = fromJust . readMayBS
  {-# INLINE readMayBS #-}
  readMayBS = BS.readInteger

instance ReadBS Float

instance ReadBS Double

instance ReadBS Char where
  {-# INLINE convertBS #-}
  convertBS = BS.head

instance ReadBS String where
  {-# INLINE convertBS #-}
  convertBS = BS.unpack

instance ReadBS BS.ByteString where
  {-# INLINE convertBS #-}
  convertBS = id

instance (ReadBS a1, ReadBS a2) => ReadBS (a1, a2) where
  {-# INLINE convertBS #-}
  convertBS !bs0 =
    let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
        !a2 = convertBS (BS.dropWhile isSpace bs1)
     in (a1, a2)
  {-# INLINE readBS #-}
  readBS = fromJust . readMayBS
  {-# INLINE readMayBS #-}
  readMayBS !bs0 = do
    (!x1, !bs1) <- readMayBS bs0
    (!x2, !bs2) <- readMayBS bs1
    Just ((x1, x2), bs2)

instance (ReadBS a1, ReadBS a2, ReadBS a3) => ReadBS (a1, a2, a3) where
  {-# INLINE convertBS #-}
  convertBS !bs0 =
    let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
        (!a2, !bs2) = readBS (BS.dropWhile isSpace bs1)
        !a3 = convertBS (BS.dropWhile isSpace bs2)
     in (a1, a2, a3)
  {-# INLINE readBS #-}
  readBS = fromJust . readMayBS
  {-# INLINE readMayBS #-}
  readMayBS !bs0 = do
    (!x1, !bs1) <- readMayBS bs0
    (!x2, !bs2) <- readMayBS bs1
    (!x3, !bs3) <- readMayBS bs2
    Just ((x1, x2, x3), bs3)

-- instance (ReadBS a1, ReadBS a2, ReadBS a3, ReadBS a4) => ReadBS (a1, a2, a3, a4) where
--   {-# INLINE convertBS #-}
--   convertBS !bs0 =
--     let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
--         (!a2, !bs2) = readBS (BS.dropWhile isSpace bs1)
--         (!a3, !bs3) = readBS (BS.dropWhile isSpace bs2)
--         !a4 = convertBS (BS.dropWhile isSpace bs3)
--      in (a1, a2, a3, a4)
--   {-# INLINE readBS #-}
--   readBS = fromJust . readMayBS
--   {-# INLINE readMayBS #-}
--   readMayBS !bs0 = do
--     (!x1, !bs1) <- readMayBS bs0
--     (!x2, !bs2) <- readMayBS bs1
--     (!x3, !bs3) <- readMayBS bs2
--     (!x4, !bs4) <- readMayBS bs3
--     Just ((x1, x2, x3, x4), bs4)

-- instance (ReadBS a1, ReadBS a2, ReadBS a3, ReadBS a4, ReadBS a5) => ReadBS (a1, a2, a3, a4, a5) where
--   {-# INLINE convertBS #-}
--   convertBS !bs0 =
--     let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
--         (!a2, !bs2) = readBS (BS.dropWhile isSpace bs1)
--         (!a3, !bs3) = readBS (BS.dropWhile isSpace bs2)
--         (!a4, !bs4) = readBS (BS.dropWhile isSpace bs3)
--         !a5 = convertBS (BS.dropWhile isSpace bs4)
--      in (a1, a2, a3, a4, a5)
--   {-# INLINE readBS #-}
--   readBS = fromJust . readMayBS
--   {-# INLINE readMayBS #-}
--   readMayBS !bs0 = do
--     (!x1, !bs1) <- readMayBS bs0
--     (!x2, !bs2) <- readMayBS bs1
--     (!x3, !bs3) <- readMayBS bs2
--     (!x4, !bs4) <- readMayBS bs3
--     (!x5, !bs5) <- readMayBS bs4
--     Just ((x1, x2, x3, x4, x5), bs5)

-- instance (ReadBS a1, ReadBS a2, ReadBS a3, ReadBS a4, ReadBS a5, ReadBS a6) => ReadBS (a1, a2, a3, a4, a5, a6) where
--   {-# INLINE convertBS #-}
--   convertBS !bs0 =
--     let (!a1, !bs1) = readBS (BS.dropWhile isSpace bs0)
--         (!a2, !bs2) = readBS (BS.dropWhile isSpace bs1)
--         (!a3, !bs3) = readBS (BS.dropWhile isSpace bs2)
--         (!a4, !bs4) = readBS (BS.dropWhile isSpace bs3)
--         (!a5, !bs5) = readBS (BS.dropWhile isSpace bs4)
--         !a6 = convertBS (BS.dropWhile isSpace bs5)
--      in (a1, a2, a3, a4, a5, a6)
--   {-# INLINE readBS #-}
--   readBS = fromJust . readMayBS
--   {-# INLINE readMayBS #-}
--   readMayBS !bs0 = do
--     (!x1, !bs1) <- readMayBS bs0
--     (!x2, !bs2) <- readMayBS bs1
--     (!x3, !bs3) <- readMayBS bs2
--     (!x4, !bs4) <- readMayBS bs3
--     (!x5, !bs5) <- readMayBS bs4
--     (!x6, !bs6) <- readMayBS bs5
--     Just ((x1, x2, x3, x4, x5, x6), bs6)

-- | Parses one line via the `ReadBS` class.
auto :: (ReadBS a) => IO a
auto = convertBS <$> BS.getLine

{-- Control --}

foldFor' :: (Foldable t) => b -> t a -> (b -> a -> b) -> b
foldFor' initial xs f = foldl' f initial xs

foldForM :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
foldForM initial xs m = foldM m initial xs

foldForM_ :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m ()
foldForM_ initial xs m = foldM_ m initial xs

mapFor :: [a] -> (a -> b) -> [b]
mapFor xs f = map f xs

mapAccumLFor :: Traversable t => a -> t b -> (a -> b -> (a, c)) -> (a, t c)
mapAccumLFor initial xs f = mapAccumL f initial xs

times :: Int -> (a -> a) -> a -> a
times n f s0 = snd $ until ((== n) . fst) (bimap succ f) (0 :: Int, s0)

{-- List --}

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = zipWith f [1 ..]

imapMaybe :: (Int -> a -> Maybe b) -> [a] -> [b]
imapMaybe f xs = mapMaybe (uncurry f) $ zip [1 ..] xs

countBy :: Foldable t1 => (t2 -> Bool) -> t1 t2 -> Int
countBy f = foldr (\a acc -> if f a then acc + 1 else acc) 0

-- リストを 前、LR区間、その後ろに3分割する
-- 0-indexed 始まり半開区間で指定する
-- >>>  splitLR 3 13 "merrychristmas"
-- ("mer","rychristma","s")
splitLR :: Int -> Int -> [a] -> ([a], [a], [a])
splitLR l r xs = do
  let (pre, remain) = splitAt l xs
      (mid, post) = splitAt (r - l) remain
  (pre, mid, post)

-- S の i 以降で f を満たすインデックスを探す
-- >>> findIndexFrom 1 (== '0') "01234567890123456789"
-- Just 10
-- >>> findIndexFrom 0 (== '0') "01234567890123456789"
-- Just 0
findIndexFrom :: Int -> (a -> Bool) -> [a] -> Maybe Int
findIndexFrom i f s =
  case findIndex f (drop i s) of
    Just j -> Just (i + j)
    Nothing -> Nothing

elemIndexFrom :: Eq a => Int -> a -> [a] -> Maybe Int
elemIndexFrom i x = findIndexFrom i (== x)

-- >>> deleteAt 2 [1, 2, 3, 4]
-- [1,2,4]
deleteAt :: Int -> [a] -> [a]
deleteAt i xs = do
  let (pre, post) = splitAt i xs
  pre ++ tail post

minimumWithDefault :: (Foldable t, Ord p) => p -> t p -> p
minimumWithDefault def xs
  | null xs = def
  | otherwise = minimum xs

maximumWithDefault :: (Foldable t, Ord p) => p -> t p -> p
maximumWithDefault def xs
  | null xs = def
  | otherwise = maximum xs

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ [] = return []
takeWhileM p (x : xs) = do
  flag <- p x
  if flag
    then do
      rest <- takeWhileM p xs
      return (x : rest)
    else return []

dropWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ [] = return []
dropWhileM p (x : xs) = do
  flag <- p x
  if flag
    then dropWhileM p xs
    else return (x : xs)

-- groupByAdjacent ··· groupBy と異なり隣の要素同士を比較する
-- >>> groupByAdjacent (<) [1,2,3,2,3,4,2,3,5,10,3,4,5]
-- [[1,2,3],[2,3,4],[2,3,5,10],[3,4,5]]
groupByAdjacent :: (a -> a -> Bool) -> [a] -> [[a]]
groupByAdjacent p = foldr f []
  where
    f x ((y : ys) : zs)
      | p x y = (x : y : ys) : zs
      | otherwise = [x] : ((y : ys) : zs)
    f x [] = [[x]]
    f _ zs = zs

-- リストを指定した n 個の、最低1つは要素が入っているグループに分割
-- >>> groupsN 2 [1 .. 3]
-- [[fromList [1,2],fromList [3]],[fromList [1,3],fromList [2]],[fromList [1],fromList [2,3]]]
groupsN :: Int -> [IS.Key] -> [[IS.IntSet]]
groupsN n = do
  let g = listArray @Array (1, n) $ replicate n IS.empty
  dfs_ (g, 1)
  where
    dfs_ (g, k) []
      | n < k = [elems g]
      | otherwise = []
    dfs_ (g, k) (x : xs) = do
      i <- [1 .. min k n]

      let g' = accum (flip IS.insert) g [(i, x)]
          k' = if k == i then k + 1 else k

      dfs_ (g', k') xs

-- 和 x を n 個の数で構成するパターンを全列挙する
-- "_____" を ["__", "_", "__"] の 3つに分割するなどに使える
-- >>> sumCombinations 3 5
-- [[1,1,3],[1,2,2],[1,3,1],[2,1,2],[2,2,1],[3,1,1]]
sumCombinations :: (Eq a, Num a, Enum a) => a -> a -> [[a]]
sumCombinations 0 _ = [[]]
sumCombinations 1 !x = [[x]]
sumCombinations !n !x = do
  !a <- [1 .. x - n + 1]
  !as <- sumCombinations (n - 1) (x - a)
  return (a : as)

-- 並び順は維持しつつ分割可能箇所で分割するパターンを全列挙する
--- >>> allPartitions [1, 2, 5]
-- [[[1],[2],[5]],[[1],[2,5]],[[1,2],[5]],[[1,2,5]]]
allPartitions :: [a] -> [[[a]]]
allPartitions as = do
  xs <- subsequences [1 .. length as - 1]

  let ((_, remain), grouped) = mapAccumL f (0, as) xs
        where
          f (i, acc) j =
            let k = j - i
             in ((j, drop k acc), take k acc)

  [grouped ++ [remain]]

-- 2N 個の要素を N 個の2ペアに分かれる方法を全列挙
-- >>> pairPermutations [1 .. 4]
-- [[(1,2),(3,4)],[(1,3),(2,4)],[(1,4),(2,3)]]
pairPermutations :: Eq a => [a] -> [[(a, a)]]
pairPermutations [] = [[]]
pairPermutations (x : xs) = do
  y <- xs
  sub <- pairPermutations (delete y xs)
  [(x, y) : sub]

-- >>> combinations 3 [1 .. 5]
-- [[1,2,3],[1,2,4],[1,2,5],[1,3,4],[1,3,5],[1,4,5],[2,3,4],[2,3,5],[2,4,5],[3,4,5]]
combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations n as@(_ : xs)
  | n == 0 = [[]]
  | n == 1 = map pure as
  | n == l = pure as
  | n > l = []
  | otherwise = run (l - 1) (n - 1) as $ combinations (n - 1) xs
  where
    l = length as

    run :: Int -> Int -> [a] -> [[a]] -> [[a]]
    run m k ys cs
      | m == k = map (ys ++) cs
      | otherwise = case take (m - k + 1) ys of
        (q : qs) -> do
          let dc = product [(m - k + 1) .. (m - 1)] `div` product [1 .. (k -1)]
          map (q :) cs ++ run (m - 1) k qs (drop dc cs)
        [] -> error "Invalid Case"

-- combinations 2 の結果をタプルのリストで返す
comb2 :: [a] -> [(a, a)]
comb2 = map f . combinations 2
  where
    f [a, b] = (a, b)
    f _ = error "assert"

ordNub :: Ord a => [a] -> [a]
ordNub xs =
  foldr
    ( \x k s ->
        if Set.member x s
          then k s
          else x : k (Set.insert x s)
    )
    (const [])
    xs
    Set.empty

{-- Tuple --}

filterOnFst :: (a -> Bool) -> [(a, b)] -> [(a, b)]
filterOnFst f = filter (f . fst)

filterOnSnd :: (b -> Bool) -> [(a, b)] -> [(a, b)]
filterOnSnd f = filter (f . snd)

findOnFst :: Foldable t => (b1 -> Bool) -> t (b1, b2) -> Maybe (b1, b2)
findOnFst f = find (f . fst)

findOnSnd :: Foldable t => (b -> Bool) -> t (a, b) -> Maybe (a, b)
findOnSnd f = find (f . snd)

combinePair :: (t1 -> t2 -> b) -> (t1, t1) -> (t2, t2) -> (b, b)
combinePair f (x0, y0) (x1, y2) = (f x0 x1, f y0 y2)
{-# INLINE combinePair #-}

add2 :: Num a => (a, a) -> (a, a) -> (a, a)
add2 = combinePair (+)
{-# INLINE add2 #-}

sub2 :: Num a => (a, a) -> (a, a) -> (a, a)
sub2 = combinePair (-)
{-# INLINE sub2 #-}

mul2 :: Num a => (a, a) -> (a, a) -> (a, a)
mul2 = combinePair (*)
{-# INLINE mul2 #-}

{-- String --}

-- | 部分文字列検索 / str の中から s にマッチする位置を探しリストで全て返す
-- >>> findString "hello" "hello, hello, world, hello"
-- [0,7,21]
findString :: (Eq a) => [a] -> [a] -> [Int]
findString s str = findIndices (isPrefixOf s) (tails str)

-- | 部分文字列の取得 0-indexed で開始位置を指定、指定した長さの部分文字列を返す
-- >>> substring 0 5 "Hello, World"
-- "Hello"
substring :: Int -> Int -> String -> String
substring start len = take len . drop start

-- | s に含まれる部分文字列をすべて返す
-- >>> substrings "TOYOTA"
-- ["T","TO","TOY","TOYO","TOYOT","TOYOTA","O","OY","OYO","OYOT","OYOTA","Y","YO","YOT","YOTA","O","OT","OTA","T","TA","A"]
substrings :: String -> [String]
substrings s = [substring i l s | i <- [0 .. n], l <- [1 .. n - i]]
  where
    n = length s

-- ByteString の substring
-- ByteString は BS.drop も BS.take も O(1)
byteSubstring :: Int -> Int -> BS.ByteString -> BS.ByteString
byteSubstring start len = BS.take len . BS.drop start

-- ByteString 含まれる全ての部分文字列を ByteString のリストで返す
byteSubstrings :: BS.ByteString -> [BS.ByteString]
byteSubstrings s = [byteSubstring i l s | i <- [0 .. n], l <- [1 .. n - i]]
  where
    n = BS.length s

-- 指定した文字の最初の一つを置換する。置換対象がないときは Nothing
-- 置換対象がないときは置換しないで返してくれればいいよというときは fromMaybe 使えば良い
-- >>> replaceFirst '?' '1' "?0?"
-- Just "10?"
-- >>> replaceFirst '?' '0' "101"
-- Nothing
-- >>> fromMaybe "101" $ replaceFirst '?' '0' "101"
-- "101"
replaceFirst :: Char -> Char -> String -> Maybe String
replaceFirst old new str =
  let (before, after) = break (== old) str
   in if null after
        then Nothing
        else Just $ before ++ [new] ++ tail after

-- >>> pad0 6 5
-- "000005"
pad0 :: Int -> Int -> String
pad0 n x = replicate (n - length x') '0' ++ x'
  where
    x' = show x

{-- Prefix Combine / Suffix Combine --}

prefixSum :: (IArray UArray e, Ix i, Num e) => (i, i) -> [e] -> UArray i e
prefixSum b xs = listArray @UArray b $ scanl' (+) 0 xs

suffixSum :: (IArray UArray e, Ix i, Num e) => (i, i) -> [e] -> UArray i e
suffixSum b xs = listArray @UArray b $ scanr (+) 0 xs

{-- Run Length --}

rle :: Eq a => [a] -> [(a, Int)]
rle = map (\x -> (head x, length x)) . group

rleBS :: BS.ByteString -> [(Char, Int)]
rleBS = map (\x -> (BS.head x, BS.length x)) . BS.group

{-- Math --}

nc2 :: Int -> Int
nc2 n = (n * (n - 1)) `div` 2

ncr :: Int -> Int -> Int
ncr n r = product (take r [n, n - 1 ..]) `div` product (take r [r, r - 1 ..])

ceildiv :: Integral a => a -> a -> a
ceildiv a b = (a + b - 1) `div` b

log2 :: (Ord a, Num a) => a -> Int
log2 n = pred . length $ takeWhile (<= n) $ iterate' (* 2) 1

log2f :: Integral a => a -> Int
log2f n = floor @Double $ logBase 2 (fromIntegral n)

-- x の 10^i の位以下を四捨五入する (最右が 0 番目)
-- >>> roundAt 0 123
-- 120
-- >>> roundAt 1 125
-- 100
-- >>> roundAt 1 2050
-- 2100
roundAt :: (Integral b, Integral a) => b -> a -> a
roundAt k x
  | m >= 5 = (x' + 10 - m) * 10 ^ k
  | otherwise = (x' - m) * 10 ^ k
  where
    x' = x `div` 10 ^ k
    m = x' `mod` 10

-- | from から to までの整数の和 O(1)
sumFromTo :: Integral a => a -> a -> a
sumFromTo a b
  | b < a = 0
  | otherwise = (b - a + 1) * (a + b) `div` 2

-- 初項a、公差 d、項数 n の等差数列の和
seqSum :: Integral a => a -> a -> a -> a
seqSum a d n = (n * (2 * a + (n - 1) * d)) `div` 2

-- >>> average 4 [3, 4, 7, 7]
-- (5,6)
average :: (Integral a, Foldable t) => a -> t a -> (a, a)
average n as = do
  let (p, q) = sum as `divMod` n
  (p, if q == 0 then p else p + 1)

-- >>> median 4 [1, 3, 3, 4]
-- 3
median :: Integral a => Int -> [a] -> a
median n xs
  | even n = (xs' !! (n `div` 2 - 1) + xs' !! (n `div` 2)) `div` 2
  | otherwise = xs' !! (n `div` 2)
  where
    xs' = sort xs

divisors :: Int -> [Int]
divisors n = sort . concat . mapMaybe f $ takeWhile (\a -> a * a <= n) [1 ..]
  where
    f a = do
      let (b, q) = n `divMod` a
      if q == 0
        then Just (bool [a, b] [a] (a == b))
        else Nothing

primeFactorize :: Int -> [Int]
primeFactorize n = unfoldr f (n, 2)
  where
    f (1, _) = Nothing
    f (m, p)
      | p ^ (2 :: Int) > m = Just (m, (1, m))
      | r == 0 = Just (p, (q, p))
      | otherwise = f (m, p + 1)
      where
        (q, r) = m `divMod` p

eratosthenes :: Int -> UArray Int Bool
eratosthenes n = runSTUArray $ do
  ps <- newArray (0, n) True
  mapM_ (\i -> writeArray ps i False) [0, 1]

  forM_ [2 .. n] $ \p -> do
    isPrime <- readArray ps p
    when isPrime $ do
      mapM_ (\i -> writeArray ps i False) [(p * 2), (p * 3) .. n]

  return ps

{-- digits --}

toBinary :: Int -> [Bool]
toBinary = unfoldr f
  where
    f 0 = Nothing
    f i = Just (q == 1, p)
      where
        (p, q) = i `divMod` 2

toDigits :: Integral a => a -> a -> [a]
toDigits n = reverse . unfoldr f
  where
    f 0 = Nothing -- 終了条件
    f x = Just (q, p) -- unfold"r"なので右がコンテキスト
      where
        (p, q) = divMod x n

fromDigits :: (Foldable t, Num a) => a -> t a -> a
fromDigits n = foldl' (\acc b -> acc * n + b) 0

{-- graph --}

graph :: Ix v => (v, v) -> [(v, e)] -> Array v [e]
graph = accumArray (flip (:)) []

invGraph :: Ix i => (i, i) -> [(i, i)] -> Array i [i]
invGraph b uvs = accumArray (flip (:)) [] b $ map swap uvs

graph2 :: Ix i => (i, i) -> [(i, i)] -> Array i [i]
graph2 b uvs = accumArray (flip (:)) [] b uvs'
  where
    uvs' = concatMap (\uv -> [uv, swap uv]) uvs

wGraph :: (Ix v) => (v, v) -> [((v, v), e)] -> Array v [(v, e)]
wGraph b uvs = accumArray (flip (:)) [] b xs
  where
    xs = map (\((u, v), w) -> (u, (v, w))) uvs

invWGraph :: (Ix v) => (v, v) -> [((v, v), e)] -> Array v [(v, e)]
invWGraph b uvs = accumArray (flip (:)) [] b xs
  where
    xs = map (\((u, v), w) -> (v, (u, w))) uvs

wGraph2 :: (Ix v) => (v, v) -> [((v, v), e)] -> Array v [(v, e)]
wGraph2 b uvs = accumArray (flip (:)) [] b xs
  where
    xs = concatMap (\((u, v), w) -> [(u, (v, w)), (v, (u, w))]) uvs

imGraph :: Foldable t => t (Int, Int) -> IM.IntMap [Int]
imGraph uvs = foldl' f IM.empty uvs'
  where
    f g (u, v) = IM.alter (Just . (:) v . fromMaybe []) u g
    uvs' = concatMap (\(u, v) -> [(u, v)]) uvs

imGraph2 :: Foldable t => t (Int, Int) -> IM.IntMap [Int]
imGraph2 uvs = foldl' f IM.empty uvs'
  where
    f g (u, v) = IM.alter (Just . (:) v . fromMaybe []) u g
    uvs' = concatMap (\(u, v) -> [(u, v), (v, u)]) uvs

{-- グラフ探索 --}

topSortM :: (MArray IOUArray Bool m, Ix v) => (v -> [v]) -> (v, v) -> [v] -> m [v]
topSortM nextStates b vs = do
  let s = (IS.fromList . map ix) vs
  visited <- newArray @IOUArray b False
  sorted <- foldM (f visited) [] vs
  -- DFS で vs に含まれない頂点も辿るのでそれは取り除く
  return $ filter (\v -> IS.member (ix v) s) sorted
  where
    ix = index b

    f visited order v = do
      seen <- readArray visited v
      if seen
        then return order
        else dfsM nextStates visited order v

    dfsM :: (MArray a Bool m, Ix v) => (v -> [v]) -> a v Bool -> [v] -> v -> m [v]
    dfsM nextStates_ visited order v = do
      writeArray visited v True
      order' <- foldM f order (nextStates_ v)
      return (v : order') -- 帰りがけ順にオーダーを記録
      where
        f context u = do
          seen <- readArray visited u
          if seen
            then return context
            else dfsM nextStates_ visited context u

-- ex) sccM g
-- [[10,8,9,11],[5,6,7,4],[2,3,1]]
sccM :: (Ix v, IArray a [v]) => a v [v] -> IO [[v]]
sccM g = do
  let g' = reverseGraph g

  (_, ros) <- componentsM (g !) (bounds g) $ range (bounds g)
  (cs, _) <- componentsM (g' !) (bounds g) $ concat ros

  return cs
  where
    reverseGraph g_ = accumArray @Array (flip (:)) [] (bounds g_) $ do
      (v, us) <- assocs g_
      [(u, v) | u <- us]

    -- 訪問済み頂点を共有しながら全ての連結成分を訪問
    -- 帰りがけ順オーダーも記録
    componentsM :: Ix v => (v -> [v]) -> (v, v) -> [v] -> IO ([[v]], [[v]])
    componentsM nextStates (l, u_) vs = do
      visited <- newArray @IOUArray (l, u_) False
      foldM (f visited) ([], []) vs
      where
        f visited context@(cs, ros) v = do
          flag <- readArray visited v
          if flag
            then return context
            else do
              (path, ro) <- dfsM nextStates visited ([], []) v
              return (path : cs, (v : ro) : ros) -- 最後にスタート地点を加える
    dfsM :: (MArray a Bool m, Ix v) => (v -> [v]) -> a v Bool -> ([v], [v]) -> v -> m ([v], [v])
    dfsM nextStates visited (path, ro) v = do
      writeArray visited v True
      (path', ro') <- foldM f (v : path, ro) (nextStates v)
      return (path', v : ro')
      where
        f context u = do
          seen <- readArray visited u
          if seen
            then return context
            else dfsM nextStates visited context u

dfs :: (Ix v) => (v -> [v]) -> (v, v) -> ([v], IS.IntSet) -> v -> ([v], IS.IntSet)
dfs nextStates b (path, visited) v = do
  let context = (v : path, IS.insert (ix v) visited)
      (path', visited') = foldl' f context (nextStates v)

  -- 帰りがけ順に何かしたいならこれをいじる
  (path', visited')
  where
    ix = index b

    f context u
      | IS.member (ix u) (snd context) = context
      | otherwise = do
        let (path', visited') = dfs nextStates b context u
        -- ひとつ戻った直後に何かしたいならここをいじる
        (path', visited')

bfs :: Ix v => (v -> [v]) -> Int -> (v, v) -> [v] -> UArray v Int
bfs nextStates initial (l, u_) v0s = runSTUArray $ do
  dist <- newArray (l, u_) initial

  forM_ v0s $ \v0 -> do
    writeArray dist v0 0

  aux (Seq.fromList v0s) dist
  return dist
  where
    aux Empty _ = return ()
    aux (v :<| queue) dist = do
      d <- readArray dist v
      us <- filterM (fmap (== initial) . readArray dist) (nextStates v)

      queue' <-
        foldM
          ( \q u -> do
              writeArray dist u (d + 1)
              return $ q |> u
          )
          queue
          us

      aux queue' dist

bfs2 ::
  forall a v.
  (IArray a v, Ix v) =>
  (v -> [v]) -> -- 次の状態への遷移関数
  Int -> -- 距離の初期値
  v -> -- 親頂点の初期値
  (v, v) -> -- 状態空間の境界
  [v] -> -- 状態遷移の開始頂点
  (UArray v Int, a v v) -- (経路数, 親頂点)
bfs2 nextStates initial root_ b v0s = runST $ do
  dist <- newArray b initial :: ST s (STUArray s v Int)
  parent <- newArray b root_ :: ST s (STArray s v v)

  forM_ v0s $ \v0 -> do
    writeArray dist v0 0

  aux (Seq.fromList v0s) (dist, parent)

  dist' <- freeze dist
  parent' <- freeze parent

  return (dist', parent')
  where
    aux Empty _ = return ()
    aux (v :<| queue) (dist, parent) = do
      d <- readArray dist v
      us <- filterM (fmap (== initial) . readArray dist) (nextStates v)

      queue' <-
        foldM
          ( \q u -> do
              writeArray dist u (d + 1)
              writeArray parent u v
              return $ q |> u
          )
          queue
          us

      aux queue' (dist, parent)

dijkstra :: (Hashable.Hashable v, Ix v) => (v -> [(v, Int)]) -> (v, v) -> [(v, Int)] -> UArray v Int
dijkstra nextStates b v0s = runSTUArray $ do
  dist <- newArray b maxBound

  for_ v0s $ \(v, w) -> do
    writeArray dist v w

  let queue = HashPSQ.fromList $ map (\(v, w) -> (v, w, ())) v0s

  aux queue dist
  return dist
  where
    aux queue dist = case HashPSQ.minView queue of
      Nothing -> return ()
      Just (v, dv, (), queue') -> do
        queue'' <-
          foldM
            ( \q (u, w) -> do
                du <- readArray dist u

                if dv + w < du
                  then do
                    writeArray dist u (dv + w)
                    let (_, q') = HashPSQ.alter (const ((), Just (dv + w, ()))) u q
                    return q'
                  else do
                    return q
            )
            queue'
            (nextStates v)
        aux queue'' dist

{-- IArray --}

-- | Constructs an immutable array using a generator function.
genArray :: (IArray a e, Ix i) => (i, i) -> (i -> e) -> a i e
genArray (l, u) f = listArray (l, u) $ map f $ range (l, u)
{-# INLINE genArray #-}

swapIArray :: (IArray a e, Ix i) => a i e -> i -> i -> a i e
swapIArray as i j = as // [(i, as ! j), (j, as ! i)]
{-# INLINE swapIArray #-}

-- 右上隅を基点としたグリッドの回転
rotateGrid :: (IArray a e, Ix b, Num b) => b -> a (b, b) e -> a (b, b) e
rotateGrid n = ixmap ((1, 1), (n, n)) (\(i, j) -> (n + 1 - j, i))

-- バケット
toBucket :: Ix i => (i, i) -> [i] -> UArray i Int
toBucket b xs = accumArray @UArray (+) (0 :: Int) b $ map (,1) xs

(!?) :: (IArray a e, Ix i) => a i e -> i -> Maybe e
(!?) arr i =
  let b = bounds arr
   in if inRange b i
        then Just (arr ! i)
        else Nothing
{-# INLINE (!?) #-}

{-- MArray --}

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray ary ix f = do
  v <- readArray ary ix
  writeArray ary ix $! f v
{-# INLINE modifyArray #-}

swapArray :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swapArray as i j = do
  a <- readArray as i
  b <- readArray as j
  writeArray as j a
  writeArray as i b
{-# INLINE swapArray #-}

updateArray :: (MArray a e m, Ix i) => (e -> e -> e) -> a i e -> i -> e -> m ()
updateArray f arr ix x = do
  v <- readArray arr ix
  writeArray arr ix $! f v x
{-# INLINE updateArray #-}

{-- bisect --}

-- | 左が false / 右が true で境界を引く
bisect :: Integral a => (a, a) -> (a -> Bool) -> (a, a)
bisect (ng, ok) f
  | abs (ok - ng) == 1 = (ng, ok)
  | f m = bisect (ng, m) f
  | otherwise = bisect (m, ok) f
  where
    m = (ok + ng) `div` 2

-- | 左が true / 右が false で境界を引く
bisect2 :: Integral a => (a, a) -> (a -> Bool) -> (a, a)
bisect2 (ok, ng) f
  | abs (ng - ok) == 1 = (ok, ng)
  | f m = bisect2 (m, ng) f
  | otherwise = bisect2 (ok, m) f
  where
    m = (ok + ng) `div` 2

bisectM :: (Monad m, Integral a) => (a, a) -> (a -> m Bool) -> m (a, a)
bisectM (ng, ok) f
  | abs (ok - ng) == 1 = return (ng, ok)
  | otherwise = do
    x <- f mid
    if x
      then bisectM (ng, mid) f
      else bisectM (mid, ok) f
  where
    mid = (ok + ng) `div` 2

lookupGE :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupGE x xs = do
  let (_, ub) = bounds xs
      ok = boundGE x xs

  if ok == succ ub
    then Nothing
    else Just (xs ! ok)

lookupGT :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupGT x xs = do
  let (_, ub) = bounds xs
      i = boundGT x xs

  if i == succ ub
    then Nothing
    else Just (xs ! i)

lookupLT :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupLT x xs = do
  let (lb, _) = bounds xs
      i = boundLT x xs

  if i == pred lb
    then Nothing
    else Just (xs ! i)

lookupLE :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> Maybe e
lookupLE x xs = do
  let (lb, _) = bounds xs
      i = boundLE x xs

  if i == pred lb
    then Nothing
    else Just (xs ! i)

boundGE :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundGE x xs = do
  let (lb, ub) = bounds xs
      (_, !ok) = bisect (pred lb, succ ub) (\i -> xs ! i >= x)
  ok

boundGT :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundGT x xs = do
  let (lb, ub) = bounds xs
      (_, !ok) = bisect (pred lb, succ ub) (\i -> xs ! i > x)
  ok

boundLT :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundLT x xs = do
  let (lb, ub) = bounds xs
      (!ng, _) = bisect (pred lb, succ ub) (\i -> xs ! i >= x)
  ng

boundLE :: (IArray a e, Ix i, Integral i, Ord e) => e -> a i e -> i
boundLE x xs = do
  let (lb, ub) = bounds xs
      (!ng, _) = bisect (pred lb, succ ub) (\i -> xs ! i > x)
  ng

{-- DP --}

-- 前方への単純な配るDP
-- ex) linearDP @UArray f (||) False (0, n) [(0, True)]
linearDP ::
  forall a v e.
  (IArray a e, Ix v) =>
  (v -> e -> [(v, e)]) ->
  (e -> e -> e) ->
  e ->
  (v, v) ->
  [(v, e)] ->
  a v e
linearDP f op initial b v0s = runST $ do
  dp <- newArray b initial :: ST s (STArray s v e)

  for_ v0s $ \(v, x) -> do
    writeArray dp v x

  for_ (range b) $ \v -> do
    x <- readArray dp v
    for_ (f v x) $ \(u, x') -> do
      when (inRange b u) $ do
        updateArray op dp u x'

  freeze dp

-- 畳み込みDP
-- 時間的遷移時に、状態空間が前の状態を引き継ぐ (accum)
-- ex) accumDP @UArray f max minBound (0, wx) [(0, 0)] wvs
accumDP ::
  ( IArray a e,
    Ix v,
    Eq e,
    Show e,
    Show v,
    Show (a v e),
    Foldable t
  ) =>
  ((v, e) -> x -> [(v, e')]) -> -- 状態遷移関数 f v / x をみて v の次の遷移可能性を返す
  (e -> e' -> e) -> -- 緩和の二項演算
  e -> -- 初期値 (0, minBound, maxBound, False など)
  (v, v) -> -- 状態空間の下界、上界
  [(v, e')] -> -- 開始時点の状態
  t x -> -- 入力 (時間遷移)
  a v e -- Array or UArray
accumDP f op initial (l, u) v0s xs = do
  let dp = accumArray op initial (l, u) v0s
  foldl' transition dp xs
  where
    transition dp x =
      accum op dp $
        filter (inRange (bounds dp) . fst) $
          concatMap (`f` x) (assocs dp)
      where
        !_ = dbg ("dp", filter ((/= initial) . snd) $ assocs dp)

-- 畳み込みDP
-- 時間的遷移時に、状態空間は前の状態を引き継がず都度リセットされる (accumArray)
-- ex) accumArrayDP @UArray f max minBound (0, wx) [(0, 0)] wvs
accumArrayDP ::
  ( IArray a e,
    Ix v,
    Eq e,
    Show e,
    Show v,
    Show (a v e),
    Foldable t
  ) =>
  ((v, e) -> x -> [(v, e')]) -> -- 状態遷移関数 f v / x をみて v の次の遷移可能性を返す
  (e -> e' -> e) -> -- 緩和の二項演算
  e -> -- 初期値 (0, minBound, maxBound, False など)
  (v, v) -> -- 状態空間の下界、上界
  [(v, e')] -> -- 開始時点の状態
  t x -> -- 入力 (時間遷移)
  a v e -- Array or UArray
accumArrayDP f op initial (l, u) v0s xs = do
  let dp = accumArray op initial (l, u) v0s
  foldl' transition dp xs
  where
    transition dp x =
      accumArray op initial (l, u) $
        filter (inRange (bounds dp) . fst) $
          concatMap (`f` x) (assocs dp)
      where
        !_ = dbg ("dp", filter ((/= initial) . snd) $ assocs dp)

-- 畳み込みDP の scanl' バージョン
accumArrayDP' ::
  ( IArray a e,
    Ix v,
    Eq e,
    Show v,
    Show e
  ) =>
  ((v, e) -> t -> [(v, e')]) -> -- 状態遷移関数 f v / x をみて v の次の遷移可能性を返す
  (e -> e' -> e) -> -- 緩和の二項演算
  e -> -- 初期値 (0, minBound, maxBound, False など)
  (v, v) -> -- 状態空間の下界、上界
  [(v, e')] -> -- 開始時点の状態
  [t] -> -- 入力 (時間遷移)
  [a v e] -- [Array] or [UArray]
accumArrayDP' f op initial (l, u) v0s xs = do
  let dp = accumArray op initial (l, u) v0s
  scanl' transition dp xs
  where
    transition dp x =
      accumArray op initial (l, u) $
        filter (inRange (bounds dp) . fst) $
          concatMap (`f` x) (assocs dp)
      where
        !_ = dbg ("dp", filter ((/= initial) . snd) $ assocs dp)

{-- しゃくとり法 --}

shakutori ::
  (Ord i, Enum i) =>
  (i, i) -> -- 全区間の始端終端
  (i -> e) -> -- getValue ... 区間位置から値を取得する関数
  (acc -> e -> acc) -> -- アキュムレータと R 位置の値の二項演算 (R が右に1つ進むときの演算)
  (acc -> e -> acc) -> -- アキュムレータと L 位置の値の逆演算 (L が右に1つ進むときの演算)
  acc -> -- 単位元 (L == R になってアキュムレータがこの値にリセットされる)
  (acc -> e -> Bool) -> -- 区間を維持できる条件
  [(i, i)] -- 条件を満たす (L, R) 区間のリスト
shakutori (start, end) getValue op invOp identity f = aux (start, start) identity
  where
    aux (l, r) acc
      | r > end = []
      | l > end = []
      | f acc (getValue r) = (l, r) : aux (l, succ r) (op acc (getValue r))
      | l == r = aux (succ l, succ r) identity
      | otherwise = aux (succ l, r) (invOp acc (getValue l))

{-- IO UnionFind --}

data UnionFind a v
  = UnionFind
      (a v v) -- 親頂点 / -1 は代表元
      (IOUArray v Int) -- 集合サイズ (代表元で検索する)
      (IORef Int) -- 連結成分数
      v -- 代表元 (representative element)

newUF :: (MArray a v IO, Ix v) => (v, v) -> v -> IO (UnionFind a v)
newUF (l, u) rep =
  UnionFind
    <$> newArray (l, u) rep
    <*> newArray (l, u) 1
    <*> newIORef (bool 0 (ix u + 1 - ix l) (u >= l))
    <*> pure rep
  where
    ix = index (l, u)

root :: (MArray a v m, Ix v) => UnionFind a v -> v -> m v
root uf@(UnionFind parent _ _ rep) x = do
  p <- readArray parent x
  if p == rep
    then return x
    else do
      p' <- root uf p
      writeArray parent x p'
      return p'

unite :: (MArray a v IO, Ix v) => UnionFind a v -> v -> v -> IO ()
unite uf@(UnionFind parent size refN _) x y = do
  x' <- root uf x
  y' <- root uf y

  when (x' /= y') $ do
    sizeX <- readArray size x'
    sizeY <- readArray size y'

    -- 併合する毎に集合が一つ減る
    modifyIORef' refN (+ (- 1))

    if sizeX > sizeY
      then do
        writeArray parent y' x'
        writeArray size x' (sizeX + sizeY)
      else do
        writeArray parent x' y'
        writeArray size y' (sizeX + sizeY)

isSame :: (MArray a v m, Ix v) => UnionFind a v -> v -> v -> m Bool
isSame uf x y = (==) <$> root uf x <*> root uf y

-- 指定した元が属する集合 (連結成分) のサイズを取得する
getSize :: (MArray a v IO, Ix v) => UnionFind a v -> v -> IO Int
getSize uf@(UnionFind _ size _ _) x = do
  y <- root uf x
  readArray size y

-- Union-Find から連結成分をグルーピングして取得する
getComponents :: (MArray a v m, Ix v) => UnionFind a v -> m [[v]]
getComponents uf@(UnionFind parent _ _ _) = do
  bx <- getBounds parent
  vs <-
    mapM
      ( \v -> do
          r <- root uf v
          return (r, v)
      )
      (range bx)

  let cs = filter (not . null) . elems $ accumArray @Array (flip (:)) [] bx vs
  return cs

getParents :: (IArray ia v, MArray a v m, Ix v) => UnionFind a v -> m (ia v v)
getParents (UnionFind parent _ _ _) = freeze parent

getSizes :: (MArray a v IO, Ix v) => UnionFind a v -> IO (UArray v Int)
getSizes (UnionFind _ size _ _) = freeze size

getNumComponents :: (MArray a v IO, Ix v) => UnionFind a v -> IO Int
getNumComponents (UnionFind _ _ refN _) = readIORef refN

{-- IntMultiSet --}

newtype IntMultiSet = MultiIntSet (IM.IntMap Int) deriving (Eq)

instance Show IntMultiSet where
  show = show . toListMS

-- >>> fromListMS [0, 1, 0, 2]
-- [0,0,1,2]
-- >>> fromListMS $ replicate 3 0
-- [0,0,0]
fromListMS :: [Int] -> IntMultiSet
fromListMS = foldl' (flip insertMS) emptyMS

toListMS :: IntMultiSet -> [Int]
toListMS = concatMap @[] (\(k, v) -> replicate v k) . coerce (IM.toList @Int)

-- >>> emptyMS
-- []
-- >>> insertMS 2 emptyMS
-- [2]
emptyMS :: IntMultiSet
emptyMS = coerce $ IM.empty @Int

-- >>> singletonMS 10
-- [10]
singletonMS :: Int -> IntMultiSet
singletonMS x = insertMS x emptyMS

-- >>> insertMS 2 (fromListMS [0, 4, 2, 0])
-- [0,0,2,2,4]
insertMS :: Int -> IntMultiSet -> IntMultiSet
insertMS x = coerce $ IM.insertWith @Int (+) x 1

-- x を n 個追加する
-- >>> insertNMS 2 5 (fromListMS [0])
-- [0,2,2,2,2,2]
insertNMS :: Int -> Int -> IntMultiSet -> IntMultiSet
insertNMS x n = coerce $ IM.insertWith @Int (+) x n

-- >>> deleteMS 2 (fromListMS [0, 2, 2])
-- [0,2]
-- >>> deleteMS 2 (fromListMS [0, 2])
-- [0]
-- >>> deleteMS 3 (fromListMS [0, 2])
-- [0,2]
deleteMS :: Int -> IntMultiSet -> IntMultiSet
deleteMS = coerce $ IM.update @Int (\k -> let k' = k - 1 in bool Nothing (Just k') $ k' > 0)

-- x を n 個削除する
-- 存在する個数以上を指定した場合は削除できる分だけ削除
-- delete 2 100 で 100 を 2 個削除
-- >>> deleteNMS 2 2 (fromListMS [0, 2, 2])
-- [0]
-- >>> deleteNMS 1 2 (fromListMS [0, 2, 2])
-- [0,2]
deleteNMS :: Int -> Int -> IntMultiSet -> IntMultiSet
deleteNMS n x = coerce $ IM.update @Int (\k -> let k' = k - n in bool Nothing (Just k') $ k' > 0) x

-- >>> deleteMinMS (fromListMS [3, 1, 3, 5])
-- [3,3,5]
deleteMinMS :: IntMultiSet -> IntMultiSet
deleteMinMS = coerce $ IM.updateMin @Int (\k -> let k' = k - 1 in bool Nothing (Just k') $ k' > 0)

-- >>> deleteMaxMS (fromListMS [3, 1, 3, 5])
-- [1,3,3]
deleteMaxMS :: IntMultiSet -> IntMultiSet
deleteMaxMS = coerce $ IM.updateMax @Int (\k -> let k' = k - 1 in bool Nothing (Just k') $ k' > 0)

-- >>> findMinMS (fromListMS [1,3,5])
-- 1
findMinMS :: IntMultiSet -> Int
findMinMS = coerce $ fst . IM.findMin @Int

-- >>> findMaxMS (fromListMS [1,3,5])
-- 5
findMaxMS :: IntMultiSet -> Int
findMaxMS = coerce $ fst . IM.findMax @Int

-- >>> memberMS 2 (fromListMS [1, 3, 5])
-- False
-- >>> memberMS 1 (fromListMS [1, 3, 5])
-- True
memberMS :: Int -> IntMultiSet -> Bool
memberMS = coerce $ IM.member @Int

-- >>> notMemberMS 2 (fromListMS [1, 3, 5])
-- True
-- >>> notMemberMS 1 (fromListMS [1, 3, 5])
-- False
notMemberMS :: Int -> IntMultiSet -> Bool
notMemberMS = coerce $ IM.notMember @Int

-- >>> countMS 0 (fromListMS [0, 0, 2])
-- 2
countMS :: Int -> IntMultiSet -> Int
countMS = coerce $ IM.findWithDefault @Int 0

-- >>> lookupLTMS 3 (fromListMS [1, 3, 5])
-- Just 1
lookupLTMS :: Int -> IntMultiSet -> Maybe Int
lookupLTMS x = coerce $ fmap fst . IM.lookupLT @Int x

-- >>> lookupGTMS 3 (fromListMS [1, 3, 5])
-- Just 5
lookupGTMS :: Int -> IntMultiSet -> Maybe Int
lookupGTMS x = coerce $ fmap fst . IM.lookupGT @Int x

-- >>> lookupLEMS 3 (fromListMS [1, 3, 5])
-- Just 3
lookupLEMS :: Int -> IntMultiSet -> Maybe Int
lookupLEMS x = coerce $ fmap fst . IM.lookupLE @Int x

-- >>> lookupGEMS 3 (fromListMS [1, 3, 5])
-- Just 5
lookupGEMS :: Int -> IntMultiSet -> Maybe Int
lookupGEMS x = coerce $ fmap fst . IM.lookupGE @Int x

-- >>> nullMS (fromListMS [])
-- True
-- >>> nullMS (fromListMS [1])
-- False
-- >>> nullMS (deleteMS 1 $ fromListMS [1])
-- True
nullMS :: IntMultiSet -> Bool
nullMS = coerce $ IM.null @Int

-- >>> sizeMS (fromListMS [0, 0, 1, 2])
-- 4
sizeMS :: IntMultiSet -> Int
sizeMS = coerce (IM.foldl' @Int (+) 0)

-- x 以下の値を k 個取得する。k 個ない場合は Nothing
-- >>> topKLE 3 3 (fromListMS [0, 0, 2, 2, 5])
-- >>> topKLE 2 5 (fromListMS [0, 0, 2, 2, 5])
-- >>> topKLE 3 0 (fromListMS [0, 0, 2, 2, 5])
-- Just [2,2,0]
-- Just [5,2]
-- Nothing
topKLE :: Int -> Int -> IntMultiSet -> Maybe [Int]
topKLE k x set = aux [] x
  where
    aux xs x_ = do
      v <- lookupLEMS x_ set

      let i = countMS v set
          l = length xs

          -- ここで i 全部展開する必要はない
          ys = xs ++ replicate (min (k - l) i) v

      if length ys < k
        then aux ys (v - 1)
        else return $ take k ys

-- x 以上の値を k 個取得する。k 個ない場合は Nothing
-- >>> topKGE 3 1 (fromListMS [0, 0, 2, 2, 5])
-- >>> topKGE 3 5 (fromListMS [0, 0, 2, 2, 5])
-- Just [2,2,5]
-- Nothing
topKGE :: Int -> Int -> IntMultiSet -> Maybe [Int]
topKGE k x set = aux [] x
  where
    aux xs x_ = do
      v <- lookupGEMS x_ set

      let i = countMS v set
          l = length xs
          ys = xs ++ replicate (min (k - l) i) v

      if length ys < k
        then aux ys (v + 1)
        else return $ take k ys

-- x 以上の値で k 番目の値を取得する
-- >>> findKthGE 0 1 (fromListMS [0, 0, 2, 2, 5])
-- >>> findKthGE 0 2 (fromListMS [0, 0, 2, 2, 5])
-- >>> findKthGE 0 3 (fromListMS [0, 0, 2, 2, 5])
-- >>> findKthGE 2 1 (fromListMS [0, 0, 2, 2, 5])
-- >>> findKthGE 1 3 (fromListMS [0, 0, 2, 2, 5])
-- >>> findKthGE 1 4 (fromListMS [0, 0, 2, 2, 5])
-- >>> findKthGE 6 1 (fromListMS [0, 0, 2, 2, 5])
-- >>> findKthGE 5 1 (fromListMS [0, 0, 2, 2, 5])
-- Just 0
-- Just 0
-- Just 2
-- Just 2
-- Just 5
-- Nothing
-- Nothing
-- Just 5
findKthGE :: Int -> Int -> IntMultiSet -> Maybe Int
findKthGE x k set = aux 0 x
  where
    aux cnt x_ = do
      v <- lookupGEMS x_ set

      let i = countMS v set
          cnt' = cnt + min (k - cnt) i

      if cnt' < k
        then aux cnt' (v + 1)
        else return v

elemsMS :: IntMultiSet -> [Int]
elemsMS = coerce $ IM.elems @Int

unionWithMS :: (Int -> Int -> Int) -> IntMultiSet -> IntMultiSet -> IntMultiSet
unionWithMS f = coerce $ IM.unionWith @Int f

differenceWithMS :: (Int -> Int -> Maybe Int) -> IntMultiSet -> IntMultiSet -> IntMultiSet
differenceWithMS f = coerce $ IM.differenceWith @Int f

-- TODO: minView, maxView, union, difference, intersection ...

{-- Binary Indexed Tree --}

type BIT a v = a Int v

newBIT :: (MArray a v m, Num v, Monad m) => (Int, Int) -> m (BIT a v)
newBIT (l, u) = newArray (l, u + 1) 0

incrementBIT :: (MArray a v m, Num v, Monad m) => BIT a v -> Int -> v -> m ()
incrementBIT tree i v = do
  (_, n) <- getBounds tree

  flip fix (n, i + 1) $ \loop (k, !j) -> do
    when (j <= n) $ do
      updateArray (+) tree j v
      loop (k, j + (j .&. (- j)))

-- 0 から i までの区間和
readBIT :: (MArray a v m, Num v, Monad m) => BIT a v -> Int -> m v
readBIT tree i =
  flip fix (0, i) $ \loop (!acc, !j) -> do
    if j < 1
      then return acc
      else do
        x <- readArray tree j
        loop (acc + x, j - (j .&. (- j)))

-- l から r までの区間和 (半開区間で指定する [1, 6) が欲しいなら rangeSumBIT bit 1 6)
rangeSumBIT :: (MArray a v m, Num v, Monad m) => BIT a v -> Int -> Int -> m v
rangeSumBIT tree l r = do
  sl <- readBIT tree l
  sr <- readBIT tree r
  return $ sr - sl

{-- 転倒数 --}

-- BITを使って転倒数を計算するアルゴリズム　O(n * log n)
-- k には要素数ではなく最大数 (空間サイズ) を指定する
-- (注意) xs に指定するリストはユニークである必要がある
-- https://scrapbox.io/pocala-kyopro/%E8%BB%A2%E5%80%92%E6%95%B0
-- >>> countInversion 7 [7,6,5,4,3,2,1]
-- 21
-- >>> countInversion 20 [20,19,2,1]
-- 6
countInversion :: Int -> [Int] -> IO Int
countInversion k xs = do
  b <- newBIT @IOUArray (0, k - 1)
  foldM (fn b) 0 $ zip [0 ..] xs
  where
    -- s ··· これまでみた数の個数 (== BITの配列上にある総和)
    -- v ··· x までの区間和
    -- (s - v)··· x より右の区間和、つまり x より大きな数の個数 / x までの転倒数
    fn b !acc (s, x) = do
      v <- readBIT b x
      incrementBIT b x 1
      return $ acc + s - v


{-- 2D CumSum --}

-- グリッドの配列から二次元累積和を構築する (速度を稼ぐため MArray を利用)
-- ((1,1), (h, w)) -> ((1, 1), (h + 1, w + 1)) になる (+1 は scanl の 0 が追加される)
fromArrayCS :: UArray (Int, Int) Int -> UArray (Int, Int) Int
fromArrayCS as = runSTUArray $ do
  s <- newArray b (0 :: Int)

  for_ (range (bounds as)) $ \(i, j) -> do
    writeArray s (i + 1, j + 1) (as ! (i, j))

  for_ [(i, j) | i <- [lh .. uh + 1], j <- [lw .. uw]] $ \(i, j) -> do
    x <- readArray s (i, j)
    modifyArray s (i, j + 1) (+ x)

  for_ [(i, j) | j <- [lw .. uw + 1], i <- [lh .. uh]] $ \(i, j) -> do
    x <- readArray s (i, j)
    modifyArray s (i + 1, j) (+ x)

  return s
  where
    ((lh, lw), (uh, uw)) = bounds as
    b = ((lh, lw), (uh + 1, uw + 1))

-- 矩形部分領域計算の抽象
rectRangeQuery :: Num a1 => ((a2, b) -> a1) -> (a2, b) -> (a2, b) -> a1
rectRangeQuery f (a, b) (c, d) = f (c, d) - f (a, d) - f (c, b) + f (a, b)

-- 左上 (a, b) 右下 (c, d) に対する2次元累積和をクエリする
-- 右半開区間でクエリする (例) queryCS s (a, b) (c + 1, d + 1)
queryCS :: UArray (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int
queryCS s = rectRangeQuery (s !)

{-- Data.Sequence --}

pushFrontSeq :: a -> Seq a -> Seq a
pushFrontSeq x xs = x <| xs
{-# INLINE pushFrontSeq #-}

pushBackSeq :: a -> Seq a -> Seq a
pushBackSeq xs x = x |> xs
{-# INLINE pushBackSeq #-}

viewFrontSeq :: Seq a -> Maybe a
viewFrontSeq Empty = Nothing
viewFrontSeq (x :<| _) = Just x
{-# INLINE viewFrontSeq #-}

viewBackSeq :: Seq a -> Maybe a
viewBackSeq Empty = Nothing
viewBackSeq (_ :|> x) = Just x
{-# INLINE viewBackSeq #-}

popFrontSeq :: Seq a -> Maybe (a, Seq a)
popFrontSeq Empty = Nothing
popFrontSeq (x :<| xs) = Just (x, xs)
{-# INLINE popFrontSeq #-}

popBackSeq :: Seq b -> Maybe (Seq b, b)
popBackSeq Empty = Nothing
popBackSeq (xs :|> x) = Just (xs, x)
{-# INLINE popBackSeq #-}

headSeq :: Seq p -> p
headSeq xs = case viewFrontSeq xs of
  Just x -> x
  Nothing -> error "empty sequence"
{-# INLINE headSeq #-}

lastSeq :: Seq p -> p
lastSeq xs = case viewBackSeq xs of
  Just x -> x
  Nothing -> error "empty sequence"
{-# INLINE lastSeq #-}

{-- trace --}
-- https://zenn.dev/toyboot4e/scraps/d173ff0b2203a0#comment-4d5890ffe42080

#ifdef DEBUG
dbg :: Show a => a -> ()
dbg !x = let !_ = traceShow x () in ()
#else
dbg :: Show a => a -> ()
dbg _ = ()
#endif

-- }}}
