{-# LANGUAGE BlockArguments #-}
module My.Conv.Grid where

import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import qualified Data.Vector.Strict as V
import My.Conv
import My.Data.Array

charGrid :: Int -> Int -> Conv (UArray (Int, Int) Char)
charGrid !h !w = do
    rows <- vector0 h ucBS
    pure $ runSTUArray do
        res <- newUArray_ ((1,1),(h,w))
        for_ [1..h] \i -> for_ [1..w] \j -> do
            writeArray res (i,j) $ (rows V.! (i-1)) `BS.index` (j-1)
        return res

intGrid :: Int -> Int -> Conv (UArray (Int, Int) Int)
intGrid !h !w = listArray ((1,1),(h,w)) <$> listN (h*w) ucInt
