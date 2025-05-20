module My.IO where

import Control.Monad
import Control.Monad.State.Strict
import Data.Array.Unboxed
import Data.Bool
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.List as L
import qualified Data.Vector.Unboxing as VU
import My.Data.Graph
import My.Data.Grid

int2 :: IO (Int, Int)
int2 = val ((,) <$> ucInt <*> ucInt)

int3 :: IO (Int, Int, Int)
int3 = val ((,,) <$> ucInt <*> ucInt <*> ucInt)

int4 :: IO (Int, Int, Int, Int)
int4 = val ((,,,) <$> ucInt <*> ucInt <*> ucInt <*> ucInt)

int2list :: Int -> IO [(Int, Int)]
int2list !n = fmap to2 <$> list2 n ucInt

int3list :: Int -> IO [(Int, Int, Int)]
int3list !n = fmap to3 <$> list2 n ucInt

int4list :: Int -> IO [(Int, Int, Int, Int)]
int4list !n = fmap to4 <$> list2 n ucInt

-- Graph
agraph :: Int -> IO (Graph Int)
agraph !n = genGraph (1,n) <$> int2list n

dgraph :: Int -> IO (Graph Int)
dgraph !n = genDiGraph (1,n) <$> int2list n

wgraph :: Int -> IO (WGraph Int Int)
wgraph !n = genWGraph (1,n) <$> int3list n

wdgraph :: Int -> IO (WGraph Int Int)
wdgraph !n = genWDiGraph (1,n) <$> int3list n

-- Grid
charGrid :: Int -> Int -> IO (UArray (Int, Int) Char)
charGrid !h !w = genGrid ((1,1),(h,w)) <$> list2 h ucChar

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
val !st = BS.getLine >>= maybe (error "Error: parse failed") (return . fst) . runStateT st

-- | read a linear data as List
list1 :: Conv a -> IO [a]
list1 !st = L.unfoldr (runStateT st) <$> BS.getLine

-- | read multi line data as List
list2 :: Int -> Conv a -> IO [[a]]
list2 !n !st = fmap (L.unfoldr (runStateT st)) <$> replicateM n BS.getLine

-- | read a line and convert to Vector
vector :: (VU.Unboxable a) => Int -> Conv a -> IO (VU.Vector a)
vector !n !st = val (VU.replicateM n st)

to2 :: [a] -> (a,a)
to2 [a,b] = (a,b)
to2 _ = error "invalid length."

to3 :: [a] -> (a,a,a)
to3 [a,b,c] = (a,b,c)
to3 _ = error "invalid length."

to4 :: [a] -> (a,a,a,a)
to4 [a,b,c,d] = (a,b,c,d)
to4 _ = error "invalid length."

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
