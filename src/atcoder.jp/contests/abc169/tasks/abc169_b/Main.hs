module Main where

import Data.List

main :: IO ()
main = do
  _ <- getLine
  as <- sort . map read . words <$> getLine :: IO [Integer]
  case head as of
    0 -> print 0
    _ -> proAndPrint (reverse as) 1

limit :: Integer
limit = 10^18

proAndPrint :: [Integer] -> Integer -> IO ()
proAndPrint [] n = print n
proAndPrint (x:xs) n = if n * x > limit
                       then print (-1)
                       else proAndPrint xs (n * x)
