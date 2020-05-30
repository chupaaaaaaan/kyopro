{-# LANGUAGE FlexibleContexts #-}
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List
import           Data.Sequence         (Seq, (<|), (><), (|>))
import qualified Data.Sequence         as Seq

getAsInt :: IO [Int]
getAsInt =  unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

main :: IO ()
main = do
  [k] <- getAsInt
  print $ solve k

-- solve' :: Int -> Int
-- solve' k = go (Seq.fromList [1,2,3,4,5,6,7,8,9]) 1
--   where go :: Seq Int -> Int -> Int
--         go Seq.Empty _ = 0
--         go (s Seq.:<| seq') n = let a = if s`mod`10/=0 then seq' |> 10*s+(s`mod`10)-1 else seq'
--                                     b = a |> 10*s+(s`mod`10)
--                                     c = if s`mod`10/=9 then b |> 10*s+(s`mod`10)+1 else b
--                                 in if k == n
--                                    then s
--                                    else go c (n+1)

solve :: Int -> Int
solve k = go (Seq.fromList [1,2,3,4,5,6,7,8,9]) 1
  where go :: Seq Int -> Int -> Int
        go sq n
          | Seq.null sq = 0
          | otherwise = let s = sq `Seq.index` 0
                            seq' = Seq.drop 1 sq
                            a = if s`mod`10/=0 then seq' |> 10*s+(s`mod`10)-1 else seq'
                            b = a |> 10*s+(s`mod`10)
                            c = if s`mod`10/=9 then b |> 10*s+(s`mod`10)+1 else b
                        in if k == n
                           then s
                           else go c (n+1)
