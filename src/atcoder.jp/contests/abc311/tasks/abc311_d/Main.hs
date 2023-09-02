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
import Data.Bifunctor (Bifunctor(bimap))
import Data.Graph.Inductive (neighbors)
import qualified Data.ByteString.Char8 as Set

-- | execute dfs on 2d-grid.
gridDfs ::
    -- | (top-left corner, bottom-right corner)
    ((Int, Int), (Int, Int)) ->
    -- | starting cell
    (Int, Int) ->
    -- | grid in searching
    Array (Int, Int) Char ->
    Int
gridDfs tlbr start grid = runST $ do

    -- visited cell?
    seen <- newArray tlbr S.empty

    countOnIce <- newSTRef 0
    go seen countOnIce R start
    go seen countOnIce D start

    readSTRef countOnIce

    where
        go :: STArray s (Int, Int) (S.Set Direction) -> STRef s Int -> Direction -> (Int, Int) -> ST s ()
        go seen countOnIce fromDir ij = do
            s <- readArray seen ij
            if S.member fromDir s then return () else do
                when (S.null s) $ modifySTRef countOnIce (+1)
                writeArray seen ij $ S.insert fromDir s
                candIce <- return . filter predIce . neiIce fromDir tlbr $ ij
                if not (null candIce)
                    then go seen countOnIce fromDir $ head candIce
                    else do candRock <- return . filter predRock . neiRock tlbr $ ij
                            forM_ candRock $ uncurry $ go seen countOnIce

        -- | search condition
        predIce ij = grid ! ij /= '#'
        predRock (_, ij) = grid ! ij /= '#'

        -- | lookup neighbours
        neiIce dir ((t, l), (b, r)) (i, j) = let (p,q) = toij dir in filter inGrid [(i+p, j+q)]
            where inGrid (i, j) = t <= i && i <= b && l <= j && j <= r

        neiRock ((t, l), (b, r)) (i, j) = filter inGrid $ [(d, (i+p, j+q))| (d, (p,q)) <- map (\x -> (x, toij x)) [R,L,U,D]]
            where inGrid (_, (i, j)) = t <= i && i <= b && l <= j && j <= r



data Direction = R | L | U | D deriving (Eq, Ord)
toij :: Direction -> (Int, Int)
toij dir = case dir of R -> (0, 1)
                       L -> (0, -1)
                       U -> (-1, 0)
                       D -> (1, 0)

main :: IO ()
main = do
    [n,m] <- list1 ucInt

    sgrid <- grid n m ucChar

    print $ gridDfs ((1,1), (n,m)) (2,2) sgrid

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
