{-# LANGUAGE CPP #-}

module My.Debug where

import Debug.Trace
import Data.Array.IArray
import qualified Data.List as L


dbg :: String -> ()
dbgS :: Show a => a -> ()
dbgGrid :: (IArray a e, Show e) => a (Int, Int) e -> ()

#ifndef JUDGE

dbg = (`trace`())

dbgS = (`traceShow`())

dbgGrid grid =
    let ((_,s),(_,e)) = bounds grid
        f xs = if null xs then Nothing else Just $ L.splitAt (e-s+1) xs
    in dbg . unlines . map (L.intercalate "\t" . map show) . L.unfoldr f . elems $ grid

#else

dbg = const ()

dbgS = const ()

dbgGrid = const ()

#endif
