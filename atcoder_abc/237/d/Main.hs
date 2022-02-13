{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Sequence (
    Seq (Empty, (:<|), (:|>)),
    ViewL (EmptyL, (:<)),
    ViewR ((:>), EmptyR),
    viewl,
    viewr,
    (<|),
    (><),
    (|>),
 )
import qualified Data.Sequence as Seq

main :: IO ()
main = do
    n <- readLn @Int
    ss <- readLnAsListWith unconsChar
    putStrLn . unwords . map show . toList . solve $ ss

toList :: Seq Int -> [Int]
toList Empty = []
toList (s :<| seq) = s : toList seq

{- |
>>> solve "LRRLR"
fromList [1,2,4,5,3,0]

>>> solve "LLLLLLL"
fromList [7,6,5,4,3,2,1,0]
-}
solve :: String -> Seq Int
solve = go (Seq.singleton 0) (1, 0)
  where
    go ::
        -- | accumulative seq
        Seq Int ->
        -- | (current i, previous position)
        (Int, Int) ->
        -- | source
        String ->
        -- | result
        Seq Int
    go seq _ [] = seq
    go seq (i, p) (a : as) = case a of
        'L' ->
            let (ls, rs) = Seq.splitAt p seq
             in go (ls <> Seq.singleton i <> rs) (i + 1, p) as
        'R' ->
            let (ls, rs) = Seq.splitAt (p + 1) seq
             in go (ls <> Seq.singleton i <> rs) (i + 1, p + 1) as

-- converter
unconsChar :: StateT BS.ByteString Maybe Char
unconsChar = StateT BS.uncons

-- 1D Data
-- list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine
