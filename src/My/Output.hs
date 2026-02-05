module My.Output where

import Data.Array.IArray
import Data.Bool
import qualified Data.List as L

printYn :: Bool -> IO ()
printYn = putStrLn . bool "No" "Yes"

printGrid :: IArray a Char => a (Int, Int) Char -> IO ()
printGrid grid = do
    let ((_,s),(_,e)) = bounds grid
        f xs = if null xs then Nothing else Just $ L.splitAt (e-s+1) xs

    putStr . unlines . L.unfoldr f . elems $ grid
