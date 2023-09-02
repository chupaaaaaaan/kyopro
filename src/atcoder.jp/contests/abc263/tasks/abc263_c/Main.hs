import Data.List
import Data.Function

main :: IO ()
main = do
  [n,m] <- map read . words <$> getLine
  putStrLn . unlines $ unwords . fmap show <$> (sort . filter ((==n) . length) . subsequences $ [1..m])

