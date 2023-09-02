module My.BinSearch where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro

-- Vector
sortedVector :: Ord a => Vector a -> Vector a
sortedVector v = V.create $ do
    mv <- V.thaw v
    sort mv
    return mv

{- |
Binary Search
https://qiita.com/drken/items/97e37dd6143e33a64c8c
[ok,ng) or (ng,ok] で考える
-}
bsearch ::
    -- | indexに対する述語
    (Int -> Bool) ->
    -- | ok: 解が存在するindex
    Int ->
    -- | ng: 解が存在しないindex
    Int ->
    Int
bsearch isOk = go
  where
    go :: Int -> Int -> Int
    go ok ng
        | abs (ok - ng) > 1 =
            let mid = (ok + ng) `div` 2
             in if isOk mid
                    then go mid ng
                    else go ok mid
        | otherwise = ok

condGT :: Ord a => V.Vector a -> a -> Int -> Bool
condGT vec key idx = key < vec V.! idx

condLT :: Ord a => V.Vector a -> a -> Int -> Bool
condLT vec key idx = key > vec V.! idx

condGE :: Ord a => V.Vector a -> a -> Int -> Bool
condGE vec key idx = key <= vec V.! idx

condLE :: Ord a => V.Vector a -> a -> Int -> Bool
condLE vec key idx = key >= vec V.! idx

main :: IO ()
main = do
    x <- readLn :: IO Int
    let vecb = V.fromList [1, 2, 3, 4, 4, 4, 4, 5, 5, 6, 6, 6, 6, 7, 8]

    putStrLn "====="
    putStrLn "idx: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14"
    putStrLn "val: 1 2 3 4 4 4 4 5 5 6  6  6  6  7  8"
    putStrLn "====="
    putStrLn $ "LT: " ++ show (bsearch (condLT vecb x) (-1) (V.length vecb))
    putStrLn $ "LE: " ++ show (bsearch (condLE vecb x) (-1) (V.length vecb))
    putStrLn $ "GE: " ++ show (bsearch (condGE vecb x) (V.length vecb) (-1))
    putStrLn $ "GT: " ++ show (bsearch (condGT vecb x) (V.length vecb) (-1))
    putStrLn "====="
