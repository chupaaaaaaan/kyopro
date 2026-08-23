{-# LANGUAGE CPP #-}

module My.Debug where

import Data.Array.IArray
#ifndef JUDGE
import Debug.Trace
import qualified Data.List as L
#endif

dbg :: String -> ()
dbgWhen :: Bool -> String -> ()
dbgS :: Show a => String -> a -> a
dbgSWhen :: Show a => Bool -> String -> a -> a
dbgGrid :: (IArray a e, Show e) => a (Int, Int) e -> a (Int, Int) e
dbgGridC :: IArray a Char => a (Int, Int) Char -> a (Int, Int) Char

#ifndef JUDGE

dbg = (`trace`())
dbgWhen p x = if p then dbg x else ()

dbgS s x = trace (s <> " = " <> show x) x
dbgSWhen p s x = if p then dbgS s x else x

dbgGrid grid = trace (gridString (L.intercalate "\t" . map show) grid) grid
dbgGridC grid = trace (gridString id grid) grid

gridString :: IArray a e => ([e] -> String) -> a (Int, Int) e -> String
gridString g grid =
    let ((_,s),(_,e)) = bounds grid
        f xs = if null xs then Nothing else Just (L.splitAt (e-s+1) xs)
    in unlines . map g . L.unfoldr f . elems $ grid

#else

dbg = const ()
dbgWhen _ = const ()

dbgS _ = id
dbgSWhen _ _ = id

dbgGrid = id
dbgGridC = id

#endif
