{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
import           Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import           Data.List

main :: IO ()
main = do
  s <- readLnAsListWith unconsChar

  putStrLn $ if solve s then "YES" else "NO"

-- "dreamd re am"
-- "dreamd re amer"
--
-- "dreame ra se"
-- "dreame ra ser"

-- "dreame rd ream"
-- "dreame rd reamer"
-- "dreame re rase"
-- "dreame re raser"
--
-- "erased re am"
-- "erased re amer"
-- "erasee ra se"
-- "erasee ra ser"
--
-- "eraser dr eam"
-- "eraser dr eamer"
-- "eraser er ase"
-- "eraser er aser"


solve :: String -> Bool
solve ss
  | null ss || ss == "dream" || ss == "dreamer" || ss == "erase" || ss == "eraser" = True
  | otherwise = let v = take 6 ss
                    u = take 2 $ drop 6 ss
                in if | v == "dreamd" -> solve $ drop 5 ss
                      | v == "dreame" && u == "ra" -> solve $ drop 5 ss
                      | v == "dreame" && (u == "rd" || u == "re") -> solve $ drop 7 ss
                      | v == "erased" || v == "erasee" -> solve $ drop 5 ss
                      | v == "eraser" -> solve $ drop 6 ss
                      | otherwise -> False


-- converter
unconsChar :: StateT BS.ByteString Maybe Char
unconsChar = StateT BS.uncons

-- 1D Data
-- for list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine
