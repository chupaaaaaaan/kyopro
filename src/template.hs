{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Control.Applicative
import Control.Monad
import Data.Array.IO
import Data.Array.Unboxed
import Data.Foldable
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.Ord
import Data.Sequence qualified as Seq
import Data.Traversable
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Unboxing qualified as VU
import Data.Vector.Unboxing.Mutable qualified as VUM
import Debug.Trace
import My.Debug
import My.IO
import My.Test.QuickCheck
import Test.QuickCheck qualified as QC

main :: IO ()
main = do
    _ <- val ucInt
    (n,k) <- int2
    as <- list1 ucInt
    av <- vector n ucInt
    xys <- int2list n

    return ()
