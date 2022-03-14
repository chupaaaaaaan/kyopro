{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List

main :: IO ()
main = do
  _ <- getLine
  (h:hs) <- readLnAsListWith unconsInt

  print $ solve h hs

solve :: Int -> [Int] -> Int
solve h hs = go hs h 0 20000
  where go [] _ a b       = min a b

        go [x] n a b      = let a' = a + (abs $ n - x)
                            in go [] x (min b a') b
        -- (i+1):(i+2):xs i i (i+1)
        go (x:y:xs) n a b = let a' = a + (abs $ n - x)
                                b' = a + (abs $ n - y)
                            in go (y:xs) x (min b a') b'


unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine

