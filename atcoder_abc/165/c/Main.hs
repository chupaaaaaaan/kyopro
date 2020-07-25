{-# LANGUAGE FlexibleContexts #-}
import           Control.Monad
import           Data.Array.IArray
import           Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as U

main :: IO ()
main = do
  [n,m,q] <- map read . words <$> getLine :: IO [Int]
  let l2t4 = \[a,b,c,d] -> (a,b,c,d)
  abcds <- map (l2t4 . map read . words) <$> replicateM q getLine :: IO [(Int,Int,Int,Int)]
  let seqAs = map (U.listArray (1,n)) $ genSeqAs n m :: [UArray Int Int]

  print $ maximum $ map (score abcds) seqAs

score :: [(Int,Int,Int,Int)] -> UArray Int Int -> Int
score abcds seqA = foldr (\(a,b,c,d) x -> if seqA!b - seqA!a == c
                                          then x + d
                                          else x) 0 abcds

genSeqAs :: Int -> Int -> [[Int]]
genSeqAs n m = go n 1
  where go 0 _   = [[]]
        go len p = [a:as | a <- [p..m], as <- go (len-1) a]
