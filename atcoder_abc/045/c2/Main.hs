{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main where

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.State.Strict
import           Data.Bits
import qualified Data.ByteString.Char8        as BS
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Proxy
import           Data.Sequence                (Seq ((:<|), (:|>)), ViewL ((:<)),
                                               ViewR ((:>)), viewl, viewr, (<|),
                                               (><), (|>))
import qualified Data.Sequence                as Seq
import qualified Data.Set                     as S
import           Data.STRef
import qualified Data.Vector                  as V
import qualified Data.Vector.Mutable          as VM
import qualified Data.Vector.Unboxing         as VU
import qualified Data.Vector.Unboxing.Mutable as VUM
import           Numeric

main :: IO ()
main = do
  s <- readLnAsListWith unconsChar

  print $ foldl' (+) 0 $ map (read @Int) $ concat $ f s

  return ()


{-| test
>>> f ""
[[]]

>>> f "1"
[["1"]]

>>> f "12"
[["12"],["1","2"]]

>>> f "123"
[["123"],["12","3"],["1","23"],["1","2","3"]]

-}

f :: [a] -> [[[a]]]
f [] = [[]]
f [x] = [[[x]]]
f (x:xs) = [g x, ([[x]]<>)] <*> f xs

{-| append 
>>> g 1 []
[[1]]

>>> g 1 [[2]]
[[1,2]]

>>> g 1 [[2],[3]]
[[1,2],[3]]

>>> fmap (g 1) [[[2,3]], [[2],[3]]]
[[[1,2,3]],[[1,2],[3]]]
-}
g :: a -> [[a]] -> [[a]]
g x [] = [[x]]
g x (xs:xss) = (x:xs):xss


-- converter
unconsChar :: StateT BS.ByteString Maybe Char
unconsChar = StateT BS.uncons

-- 1D Data
-- list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine
