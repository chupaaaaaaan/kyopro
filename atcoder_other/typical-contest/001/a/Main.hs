{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Array.IArray
import           Data.Array.ST
import           Data.Array.Unboxed
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List
import           Debug.Trace

type Grid a = UArray (Int,Int) a
type FlagGrid s = STUArray s (Int,Int) Bool


main :: IO ()
main = do
  [h,w] <- readLnAsListWith unconsInt
  css <- readLnAs2DArrayWith unconsChar h w :: IO (Grid Char)
  let (sh,sw) = fst $ head $ filter (\(_,e) -> e == 's') (assocs css)
      (gh,gw) = fst $ head $ filter (\(_,e) -> e == 'g') (assocs css)
      seen = gridDfs (h,w) (sh,sw) css

  -- print (sh,sw)
  -- print (gh,gw)
  -- putStrLn ""

  -- forM_ [1..h] $ \i -> do
  --   forM_ [1..w] $ \j -> do
  --     putStr $ show $ seen ! (i,j)
  --     putStr "\t"
  --   putStrLn ""
  putStrLn $ if seen ! (gh,gw) then "Yes" else "No"


-- グリッドグラフを扱うときは、VectorよりArrayのほうが便利そう。
gridDfs :: (Int,Int) -> (Int,Int) -> Grid Char -> UArray (Int,Int) Bool
gridDfs (h',w') (sh,sw) g = runSTUArray $ do
  seen <- newArray ((1,1),(h',w')) False
  go seen (sh,sw)
  return seen
  where go :: FlagGrid s -> (Int,Int) -> ST s ()
        go seen (h,w) = do
          if | h < 1 || h > h' || w < 1 || w > w' -> return ()
             | otherwise -> do
                 s <- readArray seen (h,w)
                 if s || g ! (h,w) == '#'
                   then return ()
                   else do
                     writeArray seen (h,w) True
                     -- 行きがけ順の処理
                     -- _ <- trace (show (h,w) <> " in:") $ return ()
                     mapM_ (go seen . (\(dh,dw) -> (h+dh,w+dw))) [(1,0),(-1,0),(0,1),(0,-1)]
                     -- 帰りがけ順の処理
                     -- trace (show (h,w) <> " out:") $ return ()

-- converter
unconsChar :: StateT BS.ByteString Maybe Char
unconsChar = StateT BS.uncons

unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

-- 1D Data
-- for list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine

-- 2D Data
-- n: number of lines
-- m: number of values per a line

-- for boxed array
readLnAs2DArrayWith :: IArray a e => StateT BS.ByteString Maybe e -> Int -> Int -> IO (a (Int,Int) e)
readLnAs2DArrayWith !st !n !m = listArray ((1,1),(n,m)) . unfoldr (runStateT st) . BS.concat <$> replicateM n BS.getLine
