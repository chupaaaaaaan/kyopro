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

getAsIntArray2D :: Int -> Int -> IO (UArray (Int,Int) Int)
getAsIntArray2D n m = U.listArray ((1,1),(n,m)) . concat <$> getAsIntLine n

main :: IO ()
main = do
  n <- readLn
  abcs <- getAsIntArray2D n 3
  let solve = solve1
      result = solve abcs n
  print $ maximum $ map (\x -> result U.! (n,x)) [1..3]

solve1 :: UArray (Int,Int) Int -> Int -> UArray (Int,Int) Int
solve1 abcs n = runSTUArray $ do
  dp <- newArray ((1,1),(n,3)) 0
  writeArray dp (1,1) $ abcs U.! (1,1)
  writeArray dp (1,2) $ abcs U.! (1,2)
  writeArray dp (1,3) $ abcs U.! (1,3)
  forM_ [2..n] $ \x -> do
    a <- readArray dp (x-1,1)
    b <- readArray dp (x-1,2)
    c <- readArray dp (x-1,3)

    writeArray dp (x,1) (abcs U.! (x,1) + max b c)
    writeArray dp (x,2) (abcs U.! (x,2) + max c a)
    writeArray dp (x,3) (abcs U.! (x,3) + max a b)

  return dp
