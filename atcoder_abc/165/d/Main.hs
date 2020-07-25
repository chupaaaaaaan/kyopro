main :: IO ()
main = do
  [a,b,n] <- map read . words <$> getLine :: IO [Int]
  print $ (a * min (b - 1) n) `div` b
