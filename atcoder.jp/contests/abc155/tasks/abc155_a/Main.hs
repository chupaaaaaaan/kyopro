import           Data.List
main :: IO ()
main = do
  [a,b,c] <- sort . map read . words <$> getLine :: IO [Int]
  putStrLn $ if a == b && b /= c || a /= b && b == c
             then "Yes"
             else "No"
