import           Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List

getAsInt :: IO [Int]
getAsInt =  unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  _ <- getLine
  as <- getAsInt
  let solve = solve2
  print $ solve as

solve2 :: [Int] -> Int
solve2 (x:xs) = evalState go $ if x >= 0 then (-x,True,0,xs) else (x,False,0,xs)

go :: State (Int,Bool,Int,[Int]) Int
go = do
  (m,b,n,xs) <- get
  case xs of
    []     | b -> return $ n - m
           | otherwise -> return $ n + m
    (x:ys) | x >= 0 && - m >=   x -> put (-x,b    ,n-m,ys) >> go
           | x >= 0 && - m <    x -> put (m ,b    ,n+x,ys) >> go
           | x <  0 && - m >= - x -> put (x ,not b,n-m,ys) >> go
           | x <  0 && - m <  - x -> put (m ,not b,n-x,ys) >> go

solve1 :: [Int] -> Int
solve1 (x:xs) = if x >= 0
               then go' (-x) True 0 xs
               else go'   x False 0 xs
  where go' m b n []
          | b         = n - m
          | otherwise = n + m
        go' m b n (y:ys)
          | y >= 0 && - m >=   y = go' (-y) b (n - m) ys
          | y >= 0 && - m <    y = go'  m b (n + y) ys
          | y <  0 && - m >= - y = go'  y (not b) (n - m) ys
          | y <  0 && - m <  - y = go'  m (not b) (n - y) ys
