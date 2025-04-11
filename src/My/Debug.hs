{-# LANGUAGE CPP #-}

module My.Debug where
import Debug.Trace
import Data.Array.IArray
import Data.List qualified as L


dbg :: String -> ()
dbgS :: Show a => a -> ()
dbgGrid :: (IArray a e, Show e) => a (Int, Int) e -> ()

#ifndef ATCODER

dbg = (`trace`())

dbgS = (`traceShow`())

dbgGrid ary = let ((s,_),(e,_)) = bounds ary
                  separate x = let sep@(l,_) = splitAt (e-s+1) x
                               in if null l then Nothing else Just sep
              in dbg . unlines . map (L.intercalate "\t" . map show) . L.unfoldr separate . elems $ ary

#else

dbg = const ()

dbgS = const ()

dbgGrid = const ()

#endif
