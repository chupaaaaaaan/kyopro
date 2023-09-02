{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxing as VU

main :: IO ()
main = do
    n <- readLn @Int
    as <- readLnAsListWith unconsInt

    print $ solve n as

solve :: Int -> [Int] -> Int
solve n as = V.foldr imp 0 $ V.map fst go
  where
    x `imp` y = unIntMod (fromIntegral x + fromIntegral y)
    go :: V.Vector (Int, Int)
    go = runST $ do
        dp <- VM.replicate n (0, 0)
        VM.write dp 0 (0, 1)

        -- マスiへ、それより前のマスから「1回で」到達するときの、サイコロを振る回数の期待値
        forM_ (zip [0 ..] as) $ \(i, ai) -> do
            let ec = expectedCount ai
            forM_ [i + 1 .. i + ai] $ \j -> do
                (v, m) <- VM.read dp j
                VM.write dp j (unIntMod . fromIntegralIntIntMod $ v + ec, m + 1)
        forM_ [0 .. n - 1] $ \i -> do
            (v, m) <- VM.read dp i
            VM.write dp i (unIntMod $ fromIntegral v * invMod (fromIntegral m), m)
        V.freeze dp

-- マスiから、別のマス(i+1〜i+A_i)に行くときの、サイコロを降る回数の期待値（の剰余）
-- >>> expectedCount 2
-- 249561089
expectedCount :: Int -> Int
expectedCount ai = unIntMod $ fromIntegral (ai + 1) * invMod (powMod (fromIntegral ai) 2)

modulus :: Int
-- modulus = 10^9 + 7
modulus = 998244353

newtype IntMod = IntMod {unIntMod :: Int}
    deriving (Eq)
    deriving newtype (VU.Unboxable)

fromIntegralIntIntMod :: Int -> IntMod
fromIntegralIntIntMod n = IntMod (n `mod` modulus)
{-# RULES
"fromIntegral/Int->IntMod"
    fromIntegral =
        fromIntegralIntIntMod
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
invMod 0 = error "inverse of 0"
invMod !x = powMod x (modulus - 2)

-- converter
unconsInt :: StateT BS.ByteString Maybe Int
unconsInt = StateT $ BS.readInt . BS.dropWhile isSpace

-- 1D Data
-- list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine
