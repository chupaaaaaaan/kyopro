main :: IO ()
main = do
  [n,r] <- map read . words <$> getLine :: IO [Int]
  print $ r + 100 * max 0 (10 - n)

