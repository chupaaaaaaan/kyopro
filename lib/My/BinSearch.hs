module My.BinSearch where

import qualified Data.Vector as V

meguruBSearch :: (Int -> Bool) -> Int -> Int -> Int
meguruBSearch isOk = go
  where go ok ng | abs (ok - ng) > 1 = let mid = (ok + ng) `div` 2
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
  let vecb = V.fromList   [1,2,3,4,4,4,4,5,5,6,6,6,6,7,8]

  putStrLn "====="
  putStrLn "idx: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14"
  putStrLn "val: 1 2 3 4 4 4 4 5 5 6  6  6  6  7  8"
  putStrLn "====="
  putStrLn $ "LT: " ++ show (meguruBSearch (condLT vecb x) (-1) (V.length vecb))
  putStrLn $ "LE: " ++ show (meguruBSearch (condLE vecb x) (-1) (V.length vecb))
  putStrLn $ "GE: " ++ show (meguruBSearch (condGE vecb x) (V.length vecb) (-1))
  putStrLn $ "GT: " ++ show (meguruBSearch (condGT vecb x) (V.length vecb) (-1))
  putStrLn "====="
