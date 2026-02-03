module My.Conv.Graph where

import My.Conv
import My.Data.Graph

ugraph :: Int -> Int -> Conv (Graph Int ())
ugraph !n !m = mkGraphWith fbAdj (1,n) <$> int2list m

dgraph :: Int -> Int -> Conv (Graph Int ())
dgraph !n !m = mkGraphWith fAdj (1,n) <$> int2list m

digraph :: Int -> Int -> Conv (Graph Int ())
digraph !n !m = mkGraphWith bAdj (1,n) <$> int2list m

wgraph :: Int -> Int -> Conv (Graph Int Int)
wgraph !n !m = mkGraphWith fbAdj (1,n) <$> int3list m

wdgraph :: Int -> Int -> Conv (Graph Int Int)
wdgraph !n !m = mkGraphWith fAdj (1,n) <$> int3list m
