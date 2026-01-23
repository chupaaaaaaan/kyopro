module My.Launcher where

import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BS
import My.IO


launch :: (a -> IO ()) -> Conv a -> IO ()
launch out solver = do
    bs <- BS.getContents
    out $ run solver bs

run :: Conv a -> BS.ByteString -> a
run !st !bs = do
    case evalStateT st bs of
        Nothing -> error "Failed to parse input: val"
        Just x -> x

