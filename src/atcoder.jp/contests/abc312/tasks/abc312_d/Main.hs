{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Array.IArray
import Data.Array.ST
import Data.Bits
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.List as L
import Data.Maybe
import Data.STRef
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as VM
import Data.Foldable

import Numeric

modulus :: Int
modulus = 998244353

newtype IntMod = IntMod Int deriving Eq

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

solve :: String -> IntMod
solve ss = runST $ do

    let jmax = length ss
        initial = V.replicate jmax 0

    (\f -> foldl (a -> Char -> a) a ByteString
                
                
        
    

    readArray dp (jmax, 0)

              

main :: IO ()
main = do
    ss <- getLine

    return ()


-- Input
-- converter
ucChar :: StateT ByteString Maybe Char
ucChar = StateT BS.uncons

ucInt :: StateT ByteString Maybe Int
ucInt = StateT (BS.readInt . BS.dropWhile isSpace)

-- | read a linear data as List
list1 ::
    -- | converter
    StateT ByteString Maybe a ->
    IO [a]
list1 !st = L.unfoldr (runStateT st) <$> BS.getLine

-- | read multi line data as List
list2 ::
    -- | number of lines (height)
    Int ->
    -- | converter
    StateT ByteString Maybe a ->
    IO [[a]]
list2 !n !st = fmap (L.unfoldr (runStateT st)) <$> replicateM n BS.getLine

-- | convert singleton List to Vector
toVec1 :: [[a]] -> Vector a
toVec1 = V.fromList . fmap (\[a] -> a)

-- | read a column Vector of order n whose element is 2-tuple
toVect :: [[a]] -> Vector (a, a)
toVect = V.fromList . fmap (\[a, b] -> (a, b))

-- | read a (h * w) grid data as Array
-- We assume that 2d-index starts from (1,1)
grid ::
    -- | number of lines (height)
    Int ->
    -- | number of values per a line (width)
    Int ->
    -- | converter
    StateT ByteString Maybe a ->
    IO (Array (Int, Int) a)
grid h w st = listArray ((1, 1), (h, w)) . concat <$> list2 h st
