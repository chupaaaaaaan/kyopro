module My.Runtime where

import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BS
import Data.Char
import My.Conv

launch :: (a -> IO b) -> Conv a -> IO b
launch out solver = do
    bs <- BS.getContents
    out $ run solver bs

readLine :: Conv a -> IO a
readLine conv = run (conv <* eofLine) <$> BS.getLine
    where eofLine = StateT $ \bs ->
              let bs' = BS.dropWhile isSpace bs
              in if BS.null bs' then Just ((), bs') else Nothing

run :: Conv a -> BS.ByteString -> a
run !st !bs = do
    case evalStateT st bs of
        Nothing -> error "Failed to parse input: val"
        Just x -> x

