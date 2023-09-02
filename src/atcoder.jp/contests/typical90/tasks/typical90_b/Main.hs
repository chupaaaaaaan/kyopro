{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = do
    n <- readLn @Int
    mapM_ putStrLn $ solve n

solve :: Int -> [String]
solve n
    | odd n = [""]
    | otherwise = go (n -1) 1 "("
  where
    go :: Int -> Int -> String -> [String]
    go k a s
        | a < 0 || a > n `div` 2 = []
        | k == 0 && a /= 0 = []
        | k == 0 && a == 0 = [s]
        | otherwise = go (k -1) (a + 1) (s <> "(") <> go (k -1) (a -1) (s <> ")")
