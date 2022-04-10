import           Control.Monad
import           Data.Array.ST
import           Data.Array.Unboxed    (UArray)
import qualified Data.Array.Unboxed    as U
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List

getAsInt :: IO [Int]
getAsInt =  unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getAsIntLine :: Int -> IO [[Int]]
getAsIntLine n = replicateM n getAsInt

-- n: number of lines
-- m: number of values per a line
getAsIntArray2D :: Int -> Int -> IO (UArray (Int,Int) Int)
getAsIntArray2D n m = U.listArray ((1,1),(n,m)) . concat <$> getAsIntLine n

main :: IO ()
main = do
  [n,w] <- getAsInt
  wvs <- getAsIntArray2D n 2
  let vMax = sum (map (\x -> wvs U.! (x,2)) [1..n])
      wMax = sum (map (\x -> wvs U.! (x,1)) [1..n])
      solve = solve1
      result = solve wMax n vMax wvs
  print $ head $ filter (\x -> w >= result U.! (n,x)) [vMax,vMax-1..0]
  -- sequence_ [print $ filter (\x -> w >= result U.! (i,x)) [vMax,vMax-1..0] | i <- [1..n]]

solve1 :: Int -> Int -> Int -> UArray (Int,Int) Int -> UArray (Int,Int) Int
solve1 wMax n' vMax wvs = runSTUArray $ do
  dp <- newArray ((0,0),(n',vMax)) wMax
  writeArray dp (0,0) 0
  forM_ [(n_,v_) | n_ <- [1..n'], v_ <- [0..vMax]] $ \(n,v) -> do
    let wtmp = wvs U.! (n,1)
        vtmp = wvs U.! (n,2)
    if v - vtmp < 0
      then readArray dp (n-1,v) >>= writeArray dp (n,v)
      else do
        a <- readArray dp (n-1,v)
        b <- readArray dp (n-1,v-vtmp)
        writeArray dp (n,v) $ min a (b+wtmp)
  return dp
