{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Array.IO
import Data.Array.Unboxed
import Data.Containers.ListUtils
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import qualified Data.IntPSQ as PSQ
import qualified Data.IntSet as IS
import qualified Data.List as L
import Data.Ord
import qualified Data.Sequence as Seq
import Data.Traversable
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Unboxing as VU
import qualified Data.Vector.Unboxing.Mutable as VUM
import Debug.Trace
import My.Debug
import My.IO
import My.Test.QuickCheck
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
    _ <- val ucInt
    (n,k) <- int2
    as <- list1 ucInt
    av <- vector n ucInt
    xys <- int2list n

    return ()
