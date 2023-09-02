{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Array.IArray
import           Data.Array.ST
import           Data.Array.Unboxed
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List


main :: IO ()
main = do
  n <- readLn :: IO Int
  ps <- readLnAsArrayWith unconsDouble n :: IO (UArray Int Double)

  let dp = solve n ps

  print $ sum $ map (\j -> dp ! (n,j)) [(n`div`2+1)..n]

solve :: Int -> UArray Int Double -> UArray (Int,Int) Double
solve n ps = runSTUArray $ do
  dp <- newArray ((0,0),(n,n)) 0.0

  writeArray dp (0,0) 1.0

  forM_ [0..n-1] $ \i -> do
    let p = ps ! (i+1)
    forM_ [0..i] $ \j -> do
      q <- readArray dp (i,j)
      q1 <- readArray dp (i+1,j+1)
      q2 <- readArray dp (i+1,j)
      writeArray dp (i+1,j+1) $ q1 + q * p
      writeArray dp (i+1,j) $ q2 + q * (1 - p)
  return dp


unconsDouble :: StateT BS.ByteString Maybe Double
unconsDouble = StateT $ parse dbl . BS.dropWhile isSpace

newtype Parser a = P (BS.ByteString -> Maybe (a, BS.ByteString))

parse :: Parser a -> BS.ByteString -> Maybe (a, BS.ByteString)
parse (P p) = p

item :: Parser Char
item = P BS.uncons

instance Functor Parser where
  fmap g p = P $ \inp -> case parse p inp of
                           Nothing       -> Nothing
                           Just (v, out) -> Just (g v, out)

instance Applicative Parser where
  pure v = P $ \inp -> Just (v, inp)

  pg <*> px = P $ \inp -> case parse pg inp of
                            Nothing       -> Nothing
                            Just (g, out) -> parse (fmap g px) out

instance Monad Parser where
  return = pure
  p >>= f = P $ \inp -> case parse p inp of
                          Nothing       -> Nothing
                          Just (v, out) -> parse (f v) out

instance Alternative Parser where
  empty = P $ const Nothing
  p <|> q = P $ \inp -> case parse p inp of
                          Nothing       -> parse q inp
                          Just (v, out) -> Just (v, out)


sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (==x)

dbl :: Parser Double
dbl = (char '-' >> (negate <$> natdot)) <|> natdot
  where natdot = (\xs _ ys -> read $ xs <> "." <> ys) <$> some digit <*> char '.' <*> some digit
          <|> read <$> some digit






-- 1D Data
-- for list
readLnAsListWith :: StateT BS.ByteString Maybe a -> IO [a]
readLnAsListWith !st = unfoldr (runStateT st) <$> BS.getLine

-- for array (boxed or unboxed)
readLnAsArrayWith :: IArray a e => StateT BS.ByteString Maybe e -> Int -> IO (a Int e)
readLnAsArrayWith !st !n = listArray (1,n) <$> readLnAsListWith st

