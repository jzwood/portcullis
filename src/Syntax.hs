module Syntax where

import Parser
import Data.Functor
import Control.Applicative
import Data.Char

-- Notes:
-- no Var (variables): instead you have functions with zero parameters
-- no Extern (external): we're just not gonna let you call external functions :/
-- TODO steal from this: https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html

type Name = String

newtype Var = Var String
  deriving (Show, Eq)

--data Stmt
  -- = Data Type
  -- | Function Name [Var] Expr
  -- | FunType Name [Type]
  -- | Pipe Name [Name] [Name] -- pipe name, input function names, output function names

-- NonZero = Real x where x /= 0

-- TODO: finish Stmt parser -- need to be able to parse the following examples
-- make a decision about colon or something else

type Mod = [Stmt]

data Stmt
  = Type Name PredicateExpr
  | Signature Name TypeExpr
  | Function Name [Var] Expr
  deriving (Show, Eq)

data PredicateExpr
  = X
  | Real Double
  | Binomial Op PredicateExpr PredicateExpr
  deriving (Eq, Show)

-- (a -> (a -> b) -> c) -> ((c -> a) -> a)
-- [["a", ["a", "b"], "c"], [["c", "a"], "a"]
-- -> a -> -> a b c -> -> c a a
data TypeExpr
  = NumType String
  | Arrow TypeExpr TypeExpr
  deriving (Show, Eq)

data Expr
  = Number Double -- 34.23
  | Ident Name  -- arg
  | Call Name [Expr]  -- add 12 45 (function invocation)
  | Guard [(Expr, Expr)]
  | BinOp Op Expr Expr  -- + 2 3
  deriving (Eq, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  | GreaterThan
  | LessThan
  | Equal
  | NotEqual
  | And
  | Or
  | Mod
  deriving (Eq, Show)

parseStmt :: Parser Stmt
parseStmt =  trimLeft
          $  parseFunc
         <|> parseType
         <|> parseSignature

parseSignature :: Parser Stmt
parseSignature = liftA2 Signature camel parseTypeExpr

-- NonZero > 0 x
parseType :: Parser Stmt
parseType = liftA2 Type pascal parsePredicateExpr

parsePredicateExpr :: Parser PredicateExpr
parsePredicateExpr
  =  trimLeft
  $  (camel $> X)
 <|> (Real <$> number)
 <|> parseBinomial

parseBinomial :: Parser PredicateExpr
parseBinomial = liftA3 Binomial parseOp parsePredicateExpr parsePredicateExpr

parseArrow :: Parser TypeExpr
parseArrow = liftA2 Arrow (word "->" *> parseTypeExpr) parseTypeExpr

parseTypeExpr :: Parser TypeExpr
parseTypeExpr
  =  trimLeft
  $ (NumType <$> alphaChars)
 <|> parseArrow

parseOp :: Parser Op
parseOp = (word "==" $> Equal)
       <|> (word "/=" $> NotEqual)
       <|> (char '>' $> GreaterThan)
       <|> (char '<' $> LessThan)
       <|> (char '&' $> And)
       <|> (char '|' $> Or)
       <|> (char '+' $> Plus)
       <|> (char '-' $> Minus)
       <|> (char '*' $> Times)
       <|> (char '/' $> Divide)
       <|> (char '%' $> Mod)

parseCall :: Parser Expr
parseCall = paren . trim $ liftA2 Call camel (zeroOrMore parseExpr)

parseBinOp :: Parser Expr
parseBinOp = liftA3 BinOp parseOp parseExpr parseExpr

parseFunc :: Parser Stmt
parseFunc = Function
         <$> camel
         <*> trimLeft (zeroOrMore $ Var <$> (trimLeft camel))
         <*  trimLeft (char '=')
         <*> trimLeft parseExpr

parseExpr :: Parser Expr
parseExpr =  trimLeft
          $  parseCall
         <|> parseBinOp
         <|> (Number <$> number)
         <|> (Ident <$> camel)

