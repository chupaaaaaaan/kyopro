{-# LANGUAGE TypeApplications           #-}
main :: IO ()
main = do
  n <- readLn @Integer

  putStrLn $ if n >= 2^31 || n < - 2^31 then "No" else "Yes"
