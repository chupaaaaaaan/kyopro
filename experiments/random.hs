{-# LANGUAGE RankNTypes #-}
import           Control.Monad
import           System.Random.MWC

data Test = Test { id  :: Int
                 , var :: Double
                 } deriving Show

randomTest :: GenIO -> IO Test
randomTest gen = Test <$> uniformR (1,200) gen <*> uniformR (0,1) gen

main :: IO ()
main = forM_ [1..5] $ \_ -> (withSystemRandom . asGenIO $ randomTest) >>= print

