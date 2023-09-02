{-# LANGUAGE BangPatterns               #-}
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8        as BS
import           Data.Char
import           Data.List

main :: IO ()
main = do
  [l,r] <- readLnAsListWith unconsInt

  print $ solve l r



solve :: Int -> Int -> Int
solve l r = head $ filter go [r-l,r-l-1..1]
    where go w = elem 1 . map (\x -> gcd x (x+w)) $ [l..r-w]

-- converter
unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

-- 1D Data
-- list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine
