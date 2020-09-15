{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
import qualified Data.Vector.Unboxing as VU

main :: IO ()
main = do
  n <- readLn :: IO Int

  print $ powMod 10 n - 2 * powMod 9 n + powMod 8 n

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

powMod :: IntMod -> Int -> IntMod
powMod !x 1 = x
powMod !x !k
  | even k = powMod (x * x) (k `div` 2)
  | otherwise = x * powMod (x * x) (k `div` 2)
