{-# LANGUAGE BlockArguments #-}
module My.IO where

import Control.Monad
import Control.Monad.State.Strict
import Data.Array.Unboxed
import Data.Bool
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.List as L
import qualified Data.Vector.Generic as VG
import My.Data.Graph
import My.Data.Grid

int2 :: IO (Int, Int)
int2 = val ((,) <$> ucInt <*> ucInt)

int3 :: IO (Int, Int, Int)
int3 = val ((,,) <$> ucInt <*> ucInt <*> ucInt)

int4 :: IO (Int, Int, Int, Int)
int4 = val ((,,,) <$> ucInt <*> ucInt <*> ucInt <*> ucInt)

int5 :: IO (Int, Int, Int, Int, Int)
int5 = val ((,,,,) <$> ucInt <*> ucInt <*> ucInt <*> ucInt <*> ucInt)

int2list :: Int -> IO [(Int, Int)]
int2list !n = fmap to2 <$> listN n ucInt

int3list :: Int -> IO [(Int, Int, Int)]
int3list !n = fmap to3 <$> listN n ucInt

int4list :: Int -> IO [(Int, Int, Int, Int)]
int4list !n = fmap to4 <$> listN n ucInt

int5list :: Int -> IO [(Int, Int, Int, Int, Int)]
int5list !n = fmap to5 <$> listN n ucInt

-- Graph
ugraph :: Int -> Int -> IO (Graph Int ())
ugraph !n !m = mkGraphWith fbAdj (1,n) <$> int2list m

dgraph :: Int -> Int -> IO (Graph Int ())
dgraph !n !m = mkGraphWith fAdj (1,n) <$> int2list m

digraph :: Int -> Int -> IO (Graph Int ())
digraph !n !m = mkGraphWith bAdj (1,n) <$> int2list m

wgraph :: Int -> Int -> IO (Graph Int Int)
wgraph !n !m = mkGraphWith fbAdj (1,n) <$> int3list m

wdgraph :: Int -> Int -> IO (Graph Int Int)
wdgraph !n !m = mkGraphWith fAdj (1,n) <$> int3list m

-- Grid
charGrid :: Int -> Int -> IO (UArray (Int, Int) Char)
charGrid !h !w = genGrid ((1,1),(h,w)) <$> listN h ucChar

intGrid :: Int -> Int -> IO (UArray (Int, Int) Int)
intGrid !h !w = genGrid ((1,1),(h,w)) <$> listN h ucInt

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

val :: Conv a -> IO a
val !st = BS.getLine >>= maybe (error "Failed to parse input: val") (return . fst) . runStateT st

-- | read a linear data as List
list1 :: Conv a -> IO [a]
list1 !st = L.unfoldr (runStateT st) <$> BS.getLine

-- | read multi line data as List
listN :: Int -> Conv a -> IO [[a]]
listN !n !st = fmap (L.unfoldr (runStateT st)) <$> replicateM n BS.getLine

-- | read a line and convert to Vector
vector0 :: (VG.Vector v a) => Int -> Conv a -> IO (v a)
vector0 !n !st = val (VG.replicateM n st)

vector1 :: (VG.Vector v a) => a -> Int -> Conv a -> IO (v a)
vector1 dummy !n !st = val do
    vec <- VG.replicateM n st
    return $ dummy `VG.cons` vec

toVec1 :: (VG.Vector v a) => a -> v a -> v a
toVec1 = VG.cons

to2 :: [a] -> (a,a)
to2 [a,b] = (a,b)
to2 _ = error "invalid length"

to3 :: [a] -> (a,a,a)
to3 [a,b,c] = (a,b,c)
to3 _ = error "invalid length"

to4 :: [a] -> (a,a,a,a)
to4 [a,b,c,d] = (a,b,c,d)
to4 _ = error "invalid length"

to5 :: [a] -> (a,a,a,a,a)
to5 [a,b,c,d,e] = (a,b,c,d,e)
to5 _ = error "invalid length"

-- Output Utility
yn :: Bool -> String
yn = bool "No" "Yes"

printYn :: Bool -> IO ()
printYn = putStrLn . yn

printGrid :: IArray a Char => a (Int, Int) Char -> IO ()
printGrid grid = do
    let ((_,s),(_,e)) = bounds grid
        f xs = if null xs then Nothing else Just $ L.splitAt (e-s+1) xs
    putStr . unlines . L.unfoldr f . elems $ grid
