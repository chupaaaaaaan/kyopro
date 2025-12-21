module My.Algorithm.DP where

import Data.Array.IArray
import Data.Foldable


execDP :: (IArray a e, Ix i) => ((i,e) -> b -> [(i,e)]) -> (e -> e -> e) -> e -> (i,i) -> [(i,e)] -> [b] -> a i e
execDP trans oper uv bnd initAssocs as =

    let ia = accumArray oper uv bnd initAssocs
        nextStatus ary a = accum oper ary $ concatMap (filter (inRange (bounds ary) . fst) . (`trans`a)) $ assocs ary

    in foldl' nextStatus ia as


execDP' :: (IArray a e, Ix i) => ((i,e) -> b -> [(i,e)]) -> (e -> e -> e) -> e -> (i,i) -> [(i,e)] -> [b] -> a i e
execDP' trans oper uv bnd initAssocs as =

    let ia = accumArray oper uv bnd initAssocs
        nextStatus ary a = accumArray oper uv bnd $ concatMap (filter (inRange (bounds ary) . fst) . (`trans`a)) $ assocs ary

    in foldl' nextStatus ia as
