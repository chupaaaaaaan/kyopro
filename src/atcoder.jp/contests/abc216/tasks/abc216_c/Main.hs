{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = readLn @Integer >>= putStrLn . reverse . solve 

solve :: Integer -> String
solve 0 = []
solve n = if even n
          then 'B' : solve (n `div` 2)
          else 'A' : solve (n - 1)
