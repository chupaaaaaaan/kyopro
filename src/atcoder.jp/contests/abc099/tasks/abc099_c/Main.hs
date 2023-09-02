{-# LANGUAGE FlexibleContexts #-}
main :: IO ()
main = do
  n <- readLn :: IO Int
  let count6 = map (count 6) [0..n]
      count9 = map (count 9) [n,n-1..0]
  print $ minimum $ zipWith (+) count6 count9


count :: Int -> Int -> Int
count unit money = let idx = ceiling $ logBase (fromIntegral unit) $ fromIntegral money
                   in  go (map (unit^) [idx,idx-1..0]) money 0
  where go _ 0 c      = c
        go (x:xs) m c = go xs (m`mod`x) (c + m`div`x)
