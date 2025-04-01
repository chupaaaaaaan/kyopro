{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
module My.Math.IntMod where

import qualified Data.Vector.Unboxing as VU

modulus :: Int
-- modulus = 10^9 + 7
modulus = 1000000007

newtype IntMod = IntMod Int
    deriving Eq deriving newtype VU.Unboxable

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

invMod :: IntMod -> IntMod
invMod 0  = error "inverse of 0"
invMod !x = powMod x (modulus - 2)

nPkMod :: Int -> Int -> IntMod
nPkMod !n' !k' = go 1 n' k'
  where go :: IntMod -> Int -> Int -> IntMod
        go !a !_ 0  = a
        go !a !n !k = go (a * fromIntegral n) (n - 1) (k - 1)

nCkMod :: Int -> Int -> IntMod
nCkMod !n !k = nPkMod n k * invMod (nPkMod k k)
