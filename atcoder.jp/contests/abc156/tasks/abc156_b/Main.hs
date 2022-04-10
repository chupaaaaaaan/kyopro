main :: IO ()
main = do
  [n,k] <- map read . words <$> getLine
  print $ solve n k 0

solve :: Int -> Int -> Int -> Int
solve 0 _ a = a
solve n k a = solve (n `div` k) k (a + 1)
