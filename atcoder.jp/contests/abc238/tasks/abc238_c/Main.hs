{-# LANGUAGE TypeApplications #-}
main :: IO ()
main = do
    n <- readLn @Integer
    print $ solve n

{- |
>>> solve 16
73

>>> solve 238
13870

>>> solve 999999999999999999
762062362

>>> solve 10
46
-}
solve :: Integer -> Integer
solve n = (`mod` 998244353) . sum . map (`sf` n) . takeWhile (\x -> 10 ^ x <= n) $ [0, 1 ..]

{- |
>>> sf 1 99
4095
-}
sf :: Int -> Integer -> Integer
sf k m =
    let l = min (10 ^ (k + 1) -1) m
        terms = l - 10 ^ k + 1
        sumN = (10 ^ k + l) * terms `div` 2
        sumG = (10 ^ k -1) * terms
     in sumN - sumG
