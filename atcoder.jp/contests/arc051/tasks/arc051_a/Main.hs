{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8      as BS
import           Data.Char
import           Data.List

main :: IO ()
main = do
  [x1,y1,r] <- readLnAsListWith unconsInt
  [x2',y2',x3',y3'] <- readLnAsListWith unconsInt

  let x2 = x2'-x1
      x3 = x3'-x1
      y2 = y2'-y1
      y3 = y3'-y1

  putStrLn $ if x2 <= -r && x3 >= r && y2 <= -r && y3 >= r
             then "NO"
             else "YES"

  putStrLn $ if x2^2+y2^2 <= r^2 && x2^2+y3^2 <= r^2 && x3^2+y2^2 <= r^2 && x3^2+y3^2 <= r^2
             then "NO"
             else "YES"


-- converter
unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

-- 1D Data
-- for list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine
