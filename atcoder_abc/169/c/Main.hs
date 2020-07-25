module Main where

main :: IO ()
main = do
  [a,b] <- words <$> getLine
  let a' = read a :: Integer
      b' = read (filter (/='.') b) :: Integer
  print $ a' * b' `div` 100
