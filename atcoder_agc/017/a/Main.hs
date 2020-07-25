{-# LANGUAGE FlexibleContexts #-}
import           Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as VU

main :: IO ()
main = do
  [n,p] <- map read . words <$> getLine
  avec <- readLnAsVecNWith n unconsInteger
  let aEvenNum = V.length $ V.filter even avec
      aOddNum = fromIntegral $ V.length $ V.filter odd avec
  print $ fromIntegral (2^aEvenNum) * if p == 0
                                      then sum $ map (comb aOddNum) [0,2..aOddNum]
                                      else sum $ map (comb aOddNum) [1,3..aOddNum]

comb :: Integer -> Integer -> Integer
comb n k = go n k 1 `div` go k k 1
  where go _ 0 a = a
        go m i a = go (m-1) (i-1) (a*m)


-- for unboxed value
unconsChar :: StateT BS.ByteString Maybe Char
unconsChar = StateT BS.uncons

unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

readLnAsUVecNWith :: VU.Unbox a => Int -> StateT BS.ByteString Maybe a -> IO (VU.Vector a)
readLnAsUVecNWith n st = VU.unfoldrN n (runStateT st) <$> BS.getLine

-- for boxed Values
unconsInteger :: StateT BS.ByteString Maybe Integer
unconsInteger = StateT $ BS.readInteger . BS.dropWhile isSpace

readLnAsVecNWith :: Int -> StateT BS.ByteString Maybe a -> IO (V.Vector a)
readLnAsVecNWith n st = V.unfoldrN n (runStateT st) <$> BS.getLine
