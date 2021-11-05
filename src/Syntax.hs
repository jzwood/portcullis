module Syntax where

import Parser
import Data.Functor
import Control.Applicative
import Data.Char hiding (chr)


{--
  | Function Name [Expr] Expr
--}


-- Notes:
-- no Var (variables): instead you have functions with zero parameters
-- no Extern (external): we're just not gonna let you call external functions :/
-- TODO steal from this: https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html

type Name = String
type Ident = String

newtype Var = Var String
  deriving (Show, Eq)

--data Stmt
  -- = Data Type
  -- | Function Name [Var] Expr
  -- | FunType Name [Type]
  -- | Pipe Name [Name] [Name] -- pipe name, input function names, output function names

-- NonZero = Real x where x /= 0

data Predicate = Predicate (Double -> Bool)

instance Show Predicate where
  show p = "Num -> Bool"

-- (a -> (a -> b) -> c) -> ((c -> a) -> a)
-- [["a", ["a", "b"], "c"], [["c", "a"], "a"]
-- -> a -> -> a b c -> -> c a a
data Signature = LitType Ident | TS Ident Signature

parseSignature :: Parser Expr
parseSignature = liftA3 SigOp (word "->" $> Sig) parseSignature parseSignature


data Stmt
  = Type Name Predicate
  | TypeSignature [Ident]
  | Function Name [Var] Expr
  deriving (Show)

data Expr
  = Number Double -- 34.23
  | Ident String  -- arg
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
  deriving (Eq, Ord, Show)

parseOp :: Parser Op
parseOp = (word "==" $> Equal)
       <|> (word "/=" $> Equal)
       <|> (char '>' $> GreaterThan)
       <|> (char '<' $> LessThan)
       <|> (char '+' $> Plus)
       <|> (char '-' $> Minus)
       <|> (char '*' $> Times)
       <|> (char '/' $> Divide)

parseCall :: Parser Expr
parseCall = liftA2 Call ident (zeroOrMore parseExpr)

parseBinOp :: Parser Expr
parseBinOp = liftA3 BinOp parseOp parseExpr parseExpr

parseFunc :: Parser Stmt
parseFunc = Function
         <$> ident
         <*> eatSpaces (zeroOrMore $ Var <$> (eatSpaces ident))
         <*  eatSpaces (char '=')
         <*> eatSpaces parseExpr

parseExpr' :: Parser Expr
parseExpr' =  parseCall
          <|> parseBinOp
          <|> (Number <$> number)
          <|> (Ident <$> ident)

parseExpr :: Parser Expr
parseExpr = eatSpaces parseExpr'

parseStmt :: Parser Stmt
parseStmt = eatSpaces parseFunc

{--
fizzbuzz : W -> B
fizzbuzz (W a) =
  { B "Fizz Buzz", and mod3 mod5
  { B "Fizz", mod3
  { B "Buzz", mod5
  { B (str a)
  where
    mod3 <- % a 3 = 0
    mod5 <- % a 5 = 0
    mod35 <= and mod3 mod5

fizzbuzz (I a) {
  S "fizz buzz", mod3&5 a
  S "fizz", mod3 a
  S "buzz", mod5 a
  S (quote a)
}

magic4 = ? n (N 1) -> (I 3); (N 2) -> (I 2)

SELECT (D a) FROM KITTY (N a) WHERE


a = b |> c |> d
a' = b' |> c' |> d'
a'' =
    |> b/2
  |>

a <| a |>





--}
