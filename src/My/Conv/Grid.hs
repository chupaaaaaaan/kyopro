{-# LANGUAGE BlockArguments #-}
module My.Conv.Grid where

import Control.Monad.ST
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as BS
import Data.Foldable
import qualified Data.Vector.Strict as V
import qualified Data.Vector.Unboxing as VU
import My.Conv
import My.Data.Array

charGrid :: Int -> Int -> Conv (UArray (Int, Int) Char)
charGrid !h !w = do
    rows <- vector0 h ucBSLine
    pure $ runSTUArray do
        res <- newUArray_ ((1,1),(h,w))
        for_ [1..h] \i -> for_ [1..w] \j -> do
            writeArray res (i,j) $ (rows V.! (i-1)) `BS.index` (j-1)
        return res

intGrid :: Int -> Int -> Conv (UArray (Int, Int) Int)
intGrid !h !w = do
    rows <- vector0 h ucBSLine
    case runST (runMaybeT (buildArr rows)) of
        Nothing -> StateT (const Nothing)
        Just arr -> pure arr

    where ucRow = vector0 w ucInt
          buildArr rows = do
              res <- lift $ newUArray_ ((1,1),(h,w))
              for_ [1..h] \i -> do
                  row <- hoistMaybe $ ucRow `evalStateT` (rows V.! (i-1))
                  lift $ for_ [1..w] \j -> writeArray res (i,j) (row VU.! (j-1))
              lift $ unsafeUFreeze res
