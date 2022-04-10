{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}
main :: IO ()
main = do
  x <- readLn :: IO Int
  let y = (x `div` 11) * 2
      z = x `mod` 11
  print $ if | z == 0 -> y
             | z > 0 && z <= 6 -> y + 1
             | otherwise -> y + 2
