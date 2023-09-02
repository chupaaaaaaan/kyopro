{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Array.IArray
import           Data.Array.ST
import           Data.Array.Unboxed
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.Int              (Int64)
import           Data.List

main :: IO ()
main = do
  [h,w] <- readLnAsListWith unconsInt
  as <- readLnAs2DArrayWith unconsChar h w :: IO (UArray (Int,Int) Char)

  let gd = gridDfs (h,w) (\g (i,j) -> g ! (i,j) == '#') as

  print $ gd ! (h,w)


type Grid a = UArray (Int,Int) a

-- グリッドグラフのDFS
-- 解くべき問題により、シグネチャは変更する
gridDfs :: (Int,Int) -> (Grid Char -> (Int,Int) -> Bool) -> Grid Char -> Array (Int,Int) IntMod
gridDfs (h',w') f g = runSTArray $ do
  dp <- newArray ((1,1),(h',w')) intmodZero :: ST s (STArray s (Int,Int) IntMod)
  writeArray dp (1,1) 1

  forM_ [2..h'] $ \h -> do
    u <- if f g (h-1,1) then return 0 else readArray dp (h-1,1)
    writeArray dp (h,1) $ if f g (h,1) then 0 else u

  forM_ [2..w'] $ \w -> do
    l <- if f g (1,w-1) then return 0 else readArray dp (1,w-1)
    writeArray dp (1,w) $ if f g (1,w) then 0 else l

  forM_ [2..h'] $ \h -> forM_ [2..w'] $ \w -> do
    l <- if f g (h,w-1) then return 0 else readArray dp (h,w-1)
    u <- if f g (h-1,w) then return 0 else readArray dp (h-1,w)
    writeArray dp (h,w) $ if f g (h,w) then 0 else l + u

  return dp

modulus :: Int64
-- modulus = 10^9 + 7
modulus = 1000000007

newtype IntMod = IntMod Int64 deriving Eq

-- fromIntegral_Int64_IntMod :: Int64 -> IntMod
-- fromIntegral_Int64_IntMod n = IntMod (n `mod` modulus)
-- {-# RULES
-- "fromIntegral/Int->IntMod"
--     fromIntegral = fromIntegral_Int64_IntMod . (fromIntegral :: Int -> Int64)
-- "fromIntegral/Int64->IntMod"
--     fromIntegral = fromIntegral_Int64_IntMod
-- #-}

instance Show IntMod where
  show (IntMod x) = show x

instance Num IntMod where
  IntMod x + IntMod y = IntMod ((x + y) `mod` modulus)
  IntMod x - IntMod y = IntMod ((x - y) `mod` modulus)
  IntMod x * IntMod y = IntMod ((x * y) `mod` modulus)
  fromInteger n = IntMod (fromInteger (n `mod` fromIntegral modulus))
  abs = undefined
  signum = undefined

intmodZero :: IntMod
intmodZero = 0 :: IntMod


-- converter
unconsChar :: StateT BS.ByteString Maybe Char
unconsChar = StateT BS.uncons

unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

-- 1D Data
-- for list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine

readLnAs2DArrayWith :: IArray a e => StateT BS.ByteString Maybe e -> Int -> Int -> IO (a (Int,Int) e)
readLnAs2DArrayWith !st !n !m = listArray ((1,1),(n,m)) . unfoldr (runStateT st) . BS.concat <$> replicateM n BS.getLine

