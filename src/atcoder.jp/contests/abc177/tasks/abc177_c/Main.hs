{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

import           Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import qualified Data.Vector.Unboxing  as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.map fromIntegral <$> readLnAsUVecWith unconsInt n :: IO (VU.Vector IntMod)

  let partsum = VU.tail $ VU.postscanr' (+) 0 as
      partprod = VU.foldl1' (+) $ VU.zipWith (*) as partsum

  print partprod

modulus :: Int
-- modulus = 10^9 + 7
modulus = 1000000007

newtype IntMod = IntMod Int deriving Eq deriving newtype VU.Unboxable

fromIntegralIntIntMod :: Int -> IntMod
fromIntegralIntIntMod n = IntMod (n `mod` modulus)
{-# RULES
"fromIntegral/Int->IntMod"
    fromIntegral = fromIntegralIntIntMod
#-}

instance Show IntMod where
  show (IntMod x) = show x

instance Num IntMod where
  IntMod x + IntMod y = IntMod ((x + y) `mod` modulus)
  IntMod x - IntMod y = IntMod ((x - y) `mod` modulus)
  IntMod x * IntMod y = IntMod ((x * y) `mod` modulus)
  fromInteger n = IntMod (fromInteger (n `mod` fromIntegral modulus))
  abs = undefined
  signum = undefined

unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

readLnAsUVecWith :: VU.Unboxable a => StateT BS.ByteString Maybe a -> Int -> IO (VU.Vector a)
readLnAsUVecWith !st !n = VU.unfoldrN n (runStateT st) <$> BS.getLine
