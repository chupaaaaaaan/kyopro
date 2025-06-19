{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NumericUnderscores #-}
module My.Math.IntMod where

import qualified Data.Vector.Unboxing as VU
import Data.Semigroup

modulus :: Int
modulus = 1_000_000_007
-- modulus = 998_244_353

newtype IntMod = IntMod Int
    deriving Eq deriving newtype VU.Unboxable

instance Show IntMod where
  show (IntMod x) = show x

instance Num IntMod where
  IntMod x + IntMod y = IntMod ((x + y) `mod` modulus)
  IntMod x - IntMod y = IntMod ((x - y) `mod` modulus)
  IntMod x * IntMod y = IntMod ((x * y) `mod` modulus)
  fromInteger n = IntMod (fromInteger (n `mod` fromIntegral modulus))
  abs = undefined
  signum = undefined

powMod :: Integral a => IntMod -> a -> IntMod
powMod !x !k = getProduct $ stimes k $ Product x

data NCKTable =
    NCK { size :: Int
        , factTable :: VU.Vector IntMod
        , invTable :: VU.Vector IntMod
        , factInvTable :: VU.Vector IntMod
        }

initNCK :: Int -> NCKTable
initNCK n = let ft = VU.constructN n $ \vec ->
                    case VU.length vec of
                        0 -> 1
                        1 -> 1
                        x -> IntMod x * vec VU.! (x - 1)
                it = VU.constructN n $ \vec ->
                    case VU.length vec of
                        0 -> 1
                        1 -> 1
                        x -> - vec VU.! (modulus `mod` x) * IntMod (modulus `div` x)
                fit = VU.constructN n $ \vec ->
                    case VU.length vec of
                        0 -> 1
                        1 -> 1
                        x -> vec VU.! (x - 1) * it VU.! x
            in NCK n ft it fit

nCkMod :: NCKTable -> Int -> Int -> IntMod
nCkMod (NCK size factTable _ factInvTable) n k
    | size < n || size < k = error "too large arguments"
    | n < k = error "invalid n and k: n < k"
    | otherwise = factTable VU.! n * factInvTable VU.! k * factInvTable VU.! (n-k)

nCkMod2 :: NCKTable -> Int -> Int -> IntMod
nCkMod2 (NCK size _ _ factInvTable) n k
    | size < n || size < k = error "too large arguments"
    | n < k = error "invalid n and k: n < k"
    | otherwise = nPkMod n k * factInvTable VU.! k

invMod :: NCKTable -> Int -> IntMod
invMod _ 0  = error "inverse of 0"
invMod (NCK size _ invTable _) n
    | size < n = error "too large arguments"
    | otherwise = invTable VU.! n

nPkMod :: Int -> Int -> IntMod
nPkMod !n' !k' = go 1 n' k'
  where go :: IntMod -> Int -> Int -> IntMod
        go !a !_ 0  = a
        go !a !n !k = go (a * fromIntegral n) (n - 1) (k - 1)
