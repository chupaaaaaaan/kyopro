{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8        as BS
import           Data.Char
import           Data.List
import           Data.Sequence                (Seq ((:<|), (:|>)), ViewL ((:<)),
                                               ViewR ((:>)), viewl, viewr, (<|),
                                               (><), (|>))
import qualified Data.Sequence                as Seq

main :: IO ()
main = do
  n <- readLn @Int
  qs <- replicateM n $ readLnAsListWith unconsInteger

  let str = solve qs Seq.empty []

  if null str then putStrLn "" else mapM_ putStrLn $ reverse str


solve ::  [[Integer]] -> Seq (Integer, Integer) -> [String] -> [String]
solve [] _ str = str
solve ([1,x,c]:qs) seq str = solve qs (seq |> (x,c)) str
solve ([2,c]:qs) seq str = let (ans, seq') = go c 0 seq
                         in solve qs seq' (show ans : str)
    where go :: Integer -> Integer -> Seq (Integer, Integer) -> (Integer, Seq (Integer, Integer))
          go _ _ Seq.Empty = error "error"
          go c k ((x,n):<|seq) = case compare c n of
                            GT -> go (c - n) (k + x * n) seq
                            EQ -> (k + x * n, seq)
                            LT -> (k + x * c, (x, n - c) <| seq)

-- converter
unconsInteger :: StateT BS.ByteString Maybe Integer
unconsInteger = StateT $ BS.readInteger . BS.dropWhile isSpace

-- 1D Data
-- list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine
