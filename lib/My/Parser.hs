module My.Parser where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Char

newtype Parser a = P (BS.ByteString -> Maybe (a, BS.ByteString))

parse :: Parser a -> BS.ByteString -> Maybe (a, BS.ByteString)
parse (P p) = p

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

item :: Parser Char
item = P BS.uncons

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string []     = return []
string (x:xs) = (:) <$> char x <*> string xs

ident :: Parser String
ident = (:) <$> lower <*> many alphanum

nat :: Parser Int
nat = read <$> some digit

space :: Parser ()
space = void $ many (sat isSpace)

int :: Parser Int
int = (char '-' >> (negate <$> nat)) <|> nat

dbl :: Parser Double
dbl = (char '-' >> (negate <$> natdot)) <|> natdot
  where natdot = (\xs _ ys -> read $ xs <> "." <> ys) <$> some digit <*> char '.' <*> some digit
          <|> read <$> some digit

token :: Parser a -> Parser a
token p = (\_ v _ -> v) <$> space <*> p <*> space

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

double :: Parser Double
double = token dbl

symbol :: String -> Parser String
symbol xs = token (string xs)
