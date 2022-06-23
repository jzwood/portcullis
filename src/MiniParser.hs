{-# LANGUAGE NamedFieldPuns #-}

module MiniParser where

import Control.Applicative
import Data.Char
import Util hiding (paren)

-- APPLICATIVE PARSER

data Cursor = Cursor { line :: Integer, col :: Integer}
  deriving (Show, Eq)

newtype ParseError = ParseError Cursor

instance Show ParseError where
  show (ParseError Cursor { line, col}) = concat ["Parse Error at line: ", show line, ", column: ", show col]

instance Semigroup Cursor where
  (<>) (Cursor l1 c1) (Cursor l2 c2) = Cursor (l1 + l2) (c1 + c2)

instance Monoid Cursor where
  mempty = Cursor 1 1 -- text editors typically start indexs at 1

nextLine :: Cursor -> Cursor
nextLine (Cursor line col) = Cursor (line + 1) 1

nextChar :: Cursor -> Cursor
nextChar (Cursor line col) = Cursor line (col + 1)

newtype Parser a = Parser { runParser :: Cursor -> String -> Either ParseError (a, Cursor, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f cursor [] = Left $ ParseError cursor
    f cursor (c:cs)
      | p c = Right (c, nextCursor, cs)
      | otherwise = Left $ ParseError cursor
      where
        nextCursor = if c == '\n' then nextLine cursor else nextChar cursor

anyChar :: Parser Char
anyChar = satisfy (const True)

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
  empty = Parser (\c _ -> Left $ ParseError c)
  Parser rp1 <|> Parser rp2 = Parser $ \c s ->
    case rp1 c s of
      Left c' -> rp2 c s
      Right acs -> Right acs

-- PARSER UTILS

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

zeroOrOne :: Parser a -> Parser [a]
zeroOrOne p = fmap pure p <|> pure []

optionalModifier :: (Parser a -> Parser a) -> Parser a -> Parser a
optionalModifier m p = m p <|> p

occurN :: Integer -> Parser a -> Parser [a]
occurN n p
   | n < 1 = pure []
   | otherwise = liftA2 (:) p (occurN (n - 1) p)

decimal :: Parser Double
decimal = (\s1 s2 -> read $ s1 ++ '.' : s2) <$> int <*> (dot *> int)
  where
    int = oneOrMore $ satisfy isDigit
    dot = char '.'

integer :: Parser Integer
integer = read <$> oneOrMore (satisfy isDigit)

number :: Parser Double
number = decimal <|> (fromIntegral <$> integer)

char :: Char -> Parser Char
char c = satisfy (== c)

word :: String -> Parser String
word = traverse char

alphaChar :: Parser Char
alphaChar = satisfy isAlpha

alphaChars :: Parser String
alphaChars = oneOrMore alphaChar

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

alphaNumChar :: Parser Char
alphaNumChar = satisfy isAlphaNum

identStartsWith :: (Char -> Bool) -> Parser String
identStartsWith char0 = liftA2 (:) (satisfy char0) (zeroOrMore alphaNumChar)

camel :: Parser String
camel = identStartsWith (isLower .|| (== '_'))

pascal :: Parser String
pascal = identStartsWith isUpper

address :: Parser String
address = identStartsWith (=='&')

wrap :: Char -> Char -> Parser a -> Parser a
wrap l r p = char l *> p <* char r

paren :: Parser a -> Parser a
paren = wrap '(' ')'

brack :: Parser a -> Parser a
brack = wrap '[' ']'

trim :: Parser a -> Parser a
trim p = spaces *> p <* spaces

trimLeft :: Parser a -> Parser a
trimLeft p = spaces *> p

optionalParens :: Parser a -> Parser a
optionalParens = optionalModifier paren . trim
