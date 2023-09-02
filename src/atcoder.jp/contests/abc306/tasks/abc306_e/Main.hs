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

import Numeric



data Heap a = E | T !Int !a !(Heap a) !(Heap a) deriving Show

-- Auxiliary Methods
fromList :: Ord a => [a] -> Heap a
fromList = foldr insert E

fromVector :: Ord a => Vector a -> Heap a
fromVector = V.foldr insert E

rank :: Heap a -> Int
rank E           = 0
rank (T r _ _ _) = r

makeT :: a -> Heap a -> Heap a -> Heap a
makeT x a b = if rank a >= rank b
              then T (rank b + 1) x a b
              else T (rank a + 1) x b a


-- Heap Methods
empty :: Ord a => Heap a
empty = E

isEmpty :: Ord a => Heap a -> Bool
isEmpty E = True
isEmpty _ = False

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (T 1 x E E)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
  = if (>=) x y
    then makeT x a1 $ merge b1 h2
    else makeT y a2 $ merge h1 b2

find :: Ord a => Heap a -> Maybe a
find E           = Nothing
find (T _ x _ _) = Just x

delete :: Ord a => Heap a -> Maybe (Heap a)
delete E           = Nothing
delete (T _ _ a b) = Just (merge a b)

pop :: Ord a => Heap a -> Maybe (a, Heap a)
pop E = Nothing
pop (T _ x a b) = Just (x, merge a b)


type MultiHeap a = (Heap a, Heap a)
push :: Ord a => a -> MultiHeap a -> MultiHeap a
push x (heap, elim) = (insert x heap, elim)

-- pop :: Ord a => MultiHeap a -> Maybe a
-- pop (heap, elim) = 





-- solve :: Int -> Int -> Int -> Vector (Int, Int) -> [Int]
-- solve n k q xys = go 1 0 empty
--     where go :: Int -> Int -> Heap Int -> Heap Int -> [Int]
--           go i sum_ heap | i <= k = let target = snd $ xys V.! (i-1)
--                                     in go (i+1) (sum_ + target) (insert target) heap



main :: IO ()
main = do

    [n,k,q] <- list1 ucInt
    xys <- list2 q ucInt


    

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
