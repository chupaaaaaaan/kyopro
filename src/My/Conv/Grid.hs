module My.Conv.Grid where

import Data.Array.Unboxed
import My.Conv

charGrid :: Int -> Int -> Conv (UArray (Int, Int) Char)
charGrid !h !w = listArray ((1,1),(h,w)) <$> listN (h*w) ucChar

intGrid :: Int -> Int -> Conv (UArray (Int, Int) Int)
intGrid !h !w = listArray ((1,1),(h,w)) <$> listN (h*w) ucInt
