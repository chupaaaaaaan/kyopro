main :: IO ()
main = do
  _ <- getLine
  as <- filter (\x -> x`mod`3/=0 && x`mod`5/=0) . filter even . map read . words <$> getLine :: IO [Int]
  putStrLn $ if null as then "APPROVED" else "DENIED"
