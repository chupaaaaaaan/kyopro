import Data.List
import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- sortBy pairComp . map (\xs -> (head xs, length xs)) . group . sort <$> replicateM n getLine :: IO [(String,Int)]
  let maxN = snd $ head ss
  _ <- mapM putStrLn $ map fst $ filter (\(_,x) -> x == maxN) ss
  return ()

pairComp :: (Ord a, Ord b) => (a,b) -> (a,b) -> Ordering
pairComp (s,x) (t,y) = case compare y x of
                         LT -> LT
                         GT -> GT
                         EQ -> compare s t
