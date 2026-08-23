module My.Output where

import Data.Array.IArray
import Data.Bool
import qualified Data.List as L

printYn :: Bool -> IO ()
printYn = putStrLn . bool "No" "Yes"

pyn :: Bool -> String
pyn = bool "No" "Yes"

printGrid :: IArray a Char => a (Int, Int) Char -> IO ()
printGrid grid = do
    let ((_,s),(_,e)) = bounds grid
        f xs = if null xs then Nothing else Just $ L.splitAt (e-s+1) xs

    putStr . unlines . L.unfoldr f . elems $ grid

printR :: Show a => [a] -> IO ()
printR = printR' show

printR' :: (a -> String) -> [a] -> IO ()
printR' f = putStrLn . unwords . map f

printC :: Show a => [a] -> IO ()
printC = printC' show

printC' :: (a -> String) -> [a] -> IO ()
printC' f = putStr . unlines . map f
