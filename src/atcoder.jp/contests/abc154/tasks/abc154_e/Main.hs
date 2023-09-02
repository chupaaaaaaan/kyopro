main :: IO ()
main = do
  -- n <- readLn :: IO Integer
  -- k <- readLn :: IO Integer
  -- let f = floor $ logBase 10 $ fromInteger n
  --     m1 = 9 - n `mod` 10
  --     m2 = 99 - n `mod` 100

  --     k1 = (if f < 1 then 0 else f)                         * 9^1 + n `div` (10^f)
  --     k2 = (if f < 2 then 0 else f * (f-1) `div` 2)         * 9^2 + n `div` (10^f) * (if f < 1 then 0 else f) * 9^1 - k1
  --     k3 = (if f < 3 then 0 else f * (f-1) * (f-2) `div` 6) * 9^3 + n `div` (10^f) * (if f < 2 then 0 else f * (f-1) `div` 2) * 9^2 - k2
  return ()
