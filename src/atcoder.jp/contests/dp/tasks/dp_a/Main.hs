import           Control.Monad
import           Data.Array.ST
import           Data.Array.Unboxed    (UArray)
import qualified Data.Array.Unboxed    as U
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List

getAsInt :: IO [Int]
getAsInt =  unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getAsIntArray1Dr :: Int -> IO (UArray Int Int)
getAsIntArray1Dr n = U.listArray (1,n) <$> getAsInt

main :: IO ()
main = do
  n <- readLn :: IO Int
  hs <- getAsIntArray1Dr n
  let solve = solve3
  print $ solve hs n

-- 貰うDP (他の点の値から、注目点での値を構成する。)
solve1 :: UArray Int Int -> Int -> Int
solve1 hs n = (U.! n) $ runSTUArray $ do
  dp <- newArray (1,n) 0
  forM_ [2..n] $ \x ->
    if x == 2
    then writeArray dp 2 . abs $ (hs U.! 2) - (hs U.! 1)
    else do
      a <- liftM2 (+) (readArray dp (x-1)) (return . abs $ hs U.! x - hs U.! (x-1))
      b <- liftM2 (+) (readArray dp (x-2)) (return . abs $ hs U.! x - hs U.! (x-2))
      writeArray dp x $ min a b
  return dp

-- 配るDP (注目点の値から、他の点での値を構成する。)
solve2 :: UArray Int Int -> Int -> Int
solve2 hs n = (U.! n) $ runSTUArray $ do
  dp <- newArray (1,n) maxBound
  writeArray dp 1 0
  forM_ [1..(n-1)] $ \x ->
    if x == n-1
    then do
      a <- liftM2 (+) (readArray dp x) (return . abs $ hs U.! x - hs U.! (x+1))
      readArray dp n >>= writeArray dp n . min a
    else do
      a <- liftM2 (+) (readArray dp x) (return . abs $ hs U.! x - hs U.! (x+1))
      b <- liftM2 (+) (readArray dp x) (return . abs $ hs U.! x - hs U.! (x+2))
      readArray dp (x+1) >>= writeArray dp (x+1) . min a
      readArray dp (x+2) >>= writeArray dp (x+2) . min b
  return dp

-- メモ化再帰はちょっとむずい。
-- 再帰しながら、ちょっとずつ配列を更新する必要があるので。
-- dp結果を格納する先を配列でないデータ構造にしたらできるかも。

-- （追記）できないっぽい。配列のindexが正格評価になるため。
-- 元データも遅延する構造で持たないとダメだな
data Tree a = T a (Tree a) (Tree a)

findTree :: Tree b -> Int -> b
findTree tree ix = f (bits $ ix + 1) tree
  where
    f []     (T v _ _) = v
    f (0:bs) (T _ l _) = f bs l
    f (_:bs) (T _ _ r) = f bs r
    bits = reverse . map (`mod`2) . takeWhile (>0) . iterate (`div`2)

genTree :: (Int -> b) -> Tree b
genTree f = gen 0
  where
    gen ix = T (f ix) (gen $ ix*2+1) (gen $ ix*2+2)

memofix :: ((Int -> b) -> (Int -> b)) -> (Int -> b)
memofix f = memof
  where
    memof = f $ findTree tbl
    tbl = genTree memof

fix :: ((Int -> b) -> (Int -> b)) -> (Int -> b)
fix f = x
  where
    x = f x

solve3 :: UArray Int Int -> Int -> Int
solve3 hs n = flip fix (n-1) $ \f i ->
  case i of
    0 -> 0
    1 -> let a = f (i-1) in a + abs (hs U.! (i+1) - hs U.! i)
    _ -> let a = f (i-1)
             b = f (i-2)
         in min (a + abs (hs U.! (i+1) - hs U.! i)) (b + abs (hs U.! (i+1) - hs U.! (i-1)))
