main :: IO ()
main = do
  _ <- getLine
  xs <- map read . words <$> getLine :: IO [Int]
  print $ minimum $ map (\x -> sum $ map (\y -> (x - y)^2) xs) [1..100]

