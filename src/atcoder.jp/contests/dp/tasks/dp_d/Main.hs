{-# LANGUAGE BangPatterns #-}

import           Control.Monad
import           Data.Array.ST
import           Data.Array.Unboxed    (UArray)
import qualified Data.Array.Unboxed    as U
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List

getAsInt :: IO [Int]
getAsInt =  unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getAsIntLine :: Int -> IO [[Int]]
getAsIntLine n = replicateM n getAsInt

getAsIntArray2D :: Int -> Int -> IO (UArray (Int,Int) Int)
getAsIntArray2D n m = U.listArray ((1,1),(n,m)) . concat <$> getAsIntLine n


main :: IO ()
main = main1

main1 :: IO ()
main1 = do
  [n,w] <- getAsInt
  wvs <- getAsIntArray2D n 2
  let solve = solve1
      result = solve n w wvs
  print $ maximum $ map (\x -> result U.! (n,x)) [0..w]

main2 :: IO ()
main2 = do
  [n,w] <- getAsInt
  wvs <- map (\[x,y] -> (x, y)) <$> getAsIntLine n
  let solve = solve2'
      result = solve w wvs
  print $ maximum $ map (\x -> result U.! x) [0..w]

solve1 :: Int -> Int -> UArray (Int,Int) Int -> UArray (Int,Int) Int
solve1 n' w' wvs = runSTUArray $ do
  dp <- newArray ((0,0),(n',w')) 0
  forM_ [(n_,w_) | n_ <- [1..n'], w_ <- [0..w']] $ \(n,w) -> do
    let wtmp = wvs U.! (n,1)
        vtmp = wvs U.! (n,2)
    if w - wtmp < 0
      then readArray dp (n-1,w) >>= writeArray dp (n,w)
      else do
        a <- readArray dp (n-1,w)
        b <- readArray dp (n-1,w-wtmp)
        writeArray dp (n,w) $ max a (b+vtmp)
  return dp

solve2 :: Int -> [(Int,Int)] -> UArray Int Int
solve2 w' [] = U.listArray (0,w') $ repeat 0
solve2 w' ((wtmp,vtmp):xs) = let a = solve2 w' xs
                                 f w = if w - wtmp >= 0
                                       then max (a U.! w) (a U.! (w - wtmp) + vtmp)
                                       else a U.! w
                             in U.array (0,w') [(w, f w) | w <- [0..w']]

solve2' :: Int -> [(Int,Int)] -> UArray Int Int
solve2' !w' [] = U.listArray (0,w') $ repeat 0
solve2' !w' ((!wtmp,!vtmp):(!xs)) = let !a = solve2' w' xs
                                        f !w = if w - wtmp >= 0
                                               then max (a U.! w) (a U.! (w - wtmp) + vtmp)
                                               else a U.! w
                                    in U.array (0,w') [(w, f w) | !w <- [0..w']]

solve3 :: Int -> [(Int,Int)] -> UArray Int Int
solve3 w' = foldr f (U.listArray (0,w') $ repeat 0)
  where f (wtmp,vtmp) a = let g w = if w - wtmp >= 0
                                    then max (a U.! w) (a U.! (w - wtmp) + vtmp)
                                    else a U.! w
                          in U.array (0,w') [(w, g w) | w <- [0..w']]

solve3' :: Int -> [(Int,Int)] -> UArray Int Int
solve3' !w' = foldr f (U.listArray (0,w') $ repeat 0)
  where f (!wtmp,!vtmp) !a = let g !w = if w - wtmp >= 0
                                        then max (a U.! w) (a U.! (w - wtmp) + vtmp)
                                        else a U.! w
                             in U.array (0,w') [(w, g w) | !w <- [0..w']]
