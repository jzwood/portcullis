{-# LANGUAGE NamedFieldPuns #-}

module Parser where

import Control.Applicative
import Data.Char

(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&) f1 f2 a = f1 a && f2 a

(.||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||) f1 f2 a = f1 a || f2 a

data Cursor = Cursor { line :: Integer, col :: Integer}
  deriving (Show)

instance Semigroup Cursor where
  (<>) (Cursor l1 c1) (Cursor l2 c2) = Cursor (l1 + l2) (c1 + c2)

instance Monoid Cursor where
  mempty = Cursor 0 0

nextLine :: Cursor -> Cursor
nextLine (Cursor line col) = Cursor (line + 1) 0

nextChar :: Cursor -> Cursor
nextChar (Cursor line col) = Cursor line (col + 1)

newtype Parser a = Parser { runParser :: Cursor -> String -> Either Cursor (a, Cursor, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f cursor [] = Left cursor
    f cursor (c:cs)
      | p c = Right (c, nextCursor, cs)
      | otherwise = Left cursor
      where
        nextCursor = if c == '\n' then nextLine cursor else nextChar cursor

char :: Char -> Parser Char
char c = satisfy (== c)

word :: String -> Parser String
word = traverse char

ffst :: (a -> b) -> (a, c, d) -> (b, c, d)
ffst f (x, y, z) = (f x, y, z)

instance Functor Parser where
 --fmap f (Parser rp) = Parser $ curry $ fmap (ffst f) . uncurry rp
 fmap f (Parser rp) = Parser (\c s -> fmap (ffst f) (rp c s))

instance Applicative Parser where
  pure a = Parser (\c s -> Right (a, c, s))
  (<*>) (Parser fp) p = Parser $ \c s ->
    case fp c s of
      Left cu -> Left cu
      Right (f, c', s') -> runParser (f <$> p) c' s'

instance Alternative Parser where
  empty = Parser (\c _ -> Left c)
  Parser rp1 <|> Parser rp2 = Parser $ \c s ->
    case rp1 c s of
      Left c' -> rp2 c s
      Right acs -> Right acs


zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

zeroOrOne :: Parser a -> Parser [a]
zeroOrOne p = undefined

decimal :: Parser Double
decimal = ((\s1 c s2 -> read $ s1 ++ c : s2) <$> (oneOrMore $ satisfy isDigit) <*> (char '.') <*> (oneOrMore $ satisfy isDigit))

integer :: Parser Integer
integer = read <$> (oneOrMore $ satisfy isDigit)

number :: Parser Double
number = decimal <|> (fromIntegral <$> integer)

chr :: Parser Char
chr = satisfy isAlpha

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> (satisfy isLower) <*> zeroOrMore (satisfy isAlphaNum)
