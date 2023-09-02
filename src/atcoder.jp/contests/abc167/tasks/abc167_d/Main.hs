{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Unboxing as VU

main :: IO ()
main = do
  [n,k] <- readLnAsListWith unconsInt
  as <- readLnAsListWith unconsInt

  print $ solve n k as

  return ()

solve :: Int -> Int -> [Int] -> Int
solve n k as =
    let maxJump = ceiling $ logBase 2 $ fromIntegral k
        seed = VU.unfoldr uncons (map (subtract 1) as)
        dp = V.unfoldrN (maxJump+1)
             (\v -> let next = VU.generate n (\i -> v VU.! (v VU.! i))
                    in Just (v, next)) seed
        go :: Int -> Int -> Int 
        go p l = if | l >= 2^maxJump         -> go (dp V.! maxJump VU.! p) (l-2^maxJump)
                    | 0 < l && l < 2^maxJump -> let jump = floor $ logBase 2 $ fromIntegral l
                                                in go (dp V.! jump VU.! p) (l-2^jump)
                    | l == 0                 -> p
                    | otherwise              -> error "don't come here."
    in 1 + go 0 k


-- converter
unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

-- 1D Data
-- list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine
