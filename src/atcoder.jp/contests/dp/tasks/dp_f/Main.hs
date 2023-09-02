import           Control.Monad
import           Data.Array.ST
import           Data.Array.Unboxed    (UArray)
import qualified Data.Array.Unboxed    as U
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as VU

main :: IO ()
main = do
  s <- BS.getLine
  t <- BS.getLine
  let solve = solve2
  putStrLn $ solve s t

table2 :: ByteString -> ByteString -> V.Vector (VU.Vector Int)
table2 xs ys = V.scanl step (VU.replicate (m+1) 0) xs'
  where n = BS.length xs
        m = BS.length ys
        xs' = V.generate n $ BS.index xs
        ys' = VU.generate m $ BS.index ys
        -- <(i-1,j)> -> i -> <(i,j)>
        step :: VU.Vector Int -> Char -> VU.Vector Int
        step v x = VU.scanl innerStep 0 $ VU.zip3 v (VU.tail v) ys'
          where
            -- (i,j-1) -> ((i-1,j-1),(i-1,j),j) -> (i,j)
            -- 第二引数はvから生成されているので、i-1 (iではなく)
            innerStep :: Int -> (Int,Int,Char) -> Int
            innerStep a (b,c,y) | x == y = b + 1
                                | otherwise = max a c

solve2 :: ByteString -> ByteString -> String
solve2 s t = go (BS.length s) (BS.length t) ""
  where tbl = table2 s t
        go 0 _ ss = ss
        go _ 0 ss = ss
        go i j ss
          | tbl V.! i VU.! j == tbl V.! (i-1) VU.! j = go (i-1) j ss
          | tbl V.! i VU.! j == tbl V.! i VU.! (j-1) = go i (j-1) ss
          | otherwise                                = go (i-1) (j-1) ((s `BS.index` (i-1)):ss)





table1 :: ByteString -> ByteString -> UArray (Int,Int) Int
table1 s t = runSTUArray $ do
  let n = BS.length s
      m = BS.length t
  dp <- newArray ((0,0),(n,m)) 0
  forM_ [1..n] $ \i -> forM_ [1..m] $ \j -> do
    a <- readArray dp (i-1,j-1)
    b <- readArray dp (i,j-1)
    c <- readArray dp (i-1,j)
    if s `BS.index` (i-1) == t `BS.index` (j-1)
      then writeArray dp (i,j) $ maximum [a+1,b,c]
      else writeArray dp (i,j) $ maximum [b,c]
  return dp

solve1 :: ByteString -> ByteString -> String
solve1 s t = go (BS.length s) (BS.length t) ""
  where ary = table1 s t
        go 0 _ ss = ss
        go _ 0 ss = ss
        go i j ss
          | ary U.! (i,j) == ary U.! (i-1,j) = go (i-1) j ss
          | ary U.! (i,j) == ary U.! (i,j-1) = go i (j-1) ss
          | otherwise                        = go (i-1) (j-1) ((s `BS.index` (i-1)):ss)
