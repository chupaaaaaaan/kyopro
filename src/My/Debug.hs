{-# LANGUAGE CPP #-}

module My.Debug where

import Debug.Trace
import Data.Array.IArray
import qualified Data.List as L


dbg :: String -> ()
dbgWhen :: Bool -> String -> ()
dbgS :: Show a => a -> String -> ()
dbgSWhen :: Show a => Bool -> a -> String -> ()
dbgGrid :: (IArray a e, Show e) => a (Int, Int) e -> ()
dbgGridWhen :: (IArray a e, Show e) => Bool -> a (Int, Int) e -> ()

#ifndef JUDGE

dbg = (`trace`())
dbgWhen p msg = if p then dbg msg else ()

dbgS x s = if null s then traceShow x () else trace (s <> show x) ()
dbgSWhen p x s = if p then dbgS x s else ()

dbgGrid grid =
    let ((_,s),(_,e)) = bounds grid
        f xs = if null xs then Nothing else Just $ L.splitAt (e-s+1) xs
    in dbg . unlines . map (L.intercalate "\t" . map show) . L.unfoldr f . elems $ grid
dbgGridWhen p g = if p then dbgGrid g else ()

#else

dbg = const ()
dbgWhen _ = const ()

dbgS _ = const ()
dbgSWhen _ _ = const ()

dbgGrid = const ()
dbgGridWhen _ = const ()

#endif
