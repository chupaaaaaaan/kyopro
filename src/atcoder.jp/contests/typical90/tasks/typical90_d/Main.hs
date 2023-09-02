{-# LANGUAGE BangPatterns #-}

import Control.Monad.State.Strict
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.List as L
import Data.Vector (Vector)
import qualified Data.Vector as V

main :: IO ()
main = do
    [h, w] <- rowl unconsInt
    ag <- gridv h w unconsInt

    flip V.imapM_ (solve h w ag) $ \i b -> do
        putStr $ show b
        if i `mod` w == w -1 then putStrLn "" else putStr " "

solve :: Int -> Int -> Vector Int -> Vector Int
solve h w ag =
    let rowSum = V.generate h (\i -> V.sum $ V.slice (i * w) w ag)
        colSum = V.generate w (\j -> V.sum $ V.generate h (\i -> ag V.! (i * w + j)))
     in V.generate (h * w) $ \k ->
            let i = k `div` w
                j = k `mod` w
             in rowSum V.! i + colSum V.! j - ag V.! k

-- Input
-- converter
unconsInt :: StateT ByteString Maybe Int
unconsInt = StateT (BS.readInt . BS.dropWhile isSpace)

-- read a linear data as List
rowl :: StateT ByteString Maybe a -> IO [a]
rowl !st = L.unfoldr (runStateT st) <$> BS.getLine

-- read a (h * w) grid data as Vector (linear-indexed)
-- n: number of lines (height)
-- m: number of values per a line (width)
-- example: if you want to access the element of (h, w) (0<=i<h, 0<=j<w), the index is i*w+j.
gridv :: Int -> Int -> StateT ByteString Maybe a -> IO (Vector a)
gridv !h !w !st = V.concatMap (V.unfoldrN w (runStateT st)) <$> V.replicateM h BS.getLine
