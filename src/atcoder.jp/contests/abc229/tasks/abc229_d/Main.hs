{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE TypeApplications           #-}
import           Control.Monad.State.Strict
import qualified Data.ByteString.Char8        as BS
import           Data.List
import qualified Data.Vector.Unboxing         as VU

main :: IO ()
main = do
  ss <- readLnAsListWith unconsChar
  k <- readLn @Int

  print $ solve ss k

solve :: [Char] -> Int -> Int
solve ss k =
    let svec = VU.unfoldr uncons ss
        go :: Int -> Int -> Int -> Int -> Int
        go a l r p
            | r == length ss = a
            | otherwise = let cl = svec VU.! l
                              cr = svec VU.! r
                          in if | cr == 'X'                                 -> go (max a (r-l+1)) l (r+1) p
                                | cr == '.' && p > 0                        -> go (max a (r-l+1)) l (r+1) (p-1)
                                | cr == '.' && p == 0 && r == l             -> go a l (r+1) 0
                                | cr == '.' && p == 0 && r > l && cl == '.' -> go a (l+1) r (min k 1)
                                | cr == '.' && p == 0 && r > l && cl == 'X' -> go a (l+1) r 0
                                | otherwise                                 -> error "don't come here."
    in go (if svec VU.! 0 == 'X' then 1 else 0) 0 0 k


-- converter
unconsChar :: StateT BS.ByteString Maybe Char
unconsChar = StateT BS.uncons

-- 1D Data
-- list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine
