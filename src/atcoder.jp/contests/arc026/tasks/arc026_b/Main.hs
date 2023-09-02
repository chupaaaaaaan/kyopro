{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

main :: IO ()
main = do
  n <- readLn @Int

  let (_:rest) = divisors n
      perf = sum rest

  putStrLn $ if | n == perf -> "Perfect"
                | n > perf -> "Deficient"
                | n < perf -> "Abundant"


-- 与えられた整数の約数を全列挙する（O(\sqrt n)）
divisors :: Int -> [Int]
divisors n = go 1
  where go f | f * f > n = []
             | f * f == n = [f]
             | otherwise = if n`mod`f == 0
                           then (n`div`f) : f : go (f+1)
                           else go (f+1)
