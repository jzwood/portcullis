module Parser where

import MiniParser
import Syntax
import Data.Functor
import Control.Applicative
import Data.Char
import Data.List
import Util

parseStmt :: Parser Stmt
parseStmt =  trimLeft
          $  parseFunc
         <|> parseSignature

parseSignature :: Parser Stmt
parseSignature = liftA2 Signature camel parseTypeExpr

parseArrow :: Parser TypeExpr
parseArrow = liftA2 Arrow (word "->" *> parseTypeExpr) parseTypeExpr

parseFunc :: Parser Stmt
parseFunc = Function
         <$> camel
         <*> trimLeft (zeroOrMore $ Var <$> (trimLeft camel))
         <*  trimLeft (char '=')
         <*> trimLeft parseExpr

parseTypeExpr :: Parser TypeExpr
parseTypeExpr
  =  trimLeft
  $ (NumType <$> alphaChars)
 <|> parseArrow

parseBop :: Parser Bop
parseBop = (word "==" $> Equal)
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

parseTop :: Parser Top
parseTop =  (word "fold" $> Fold)
        <|> (word "unfold" $> Unfold)

parseCall :: Parser Expr
parseCall = paren . trim
          $ liftA2 Call camel (zeroOrMore parseExpr)

parseBinOp :: Parser Expr
parseBinOp = (optionalModifier paren . trim)
           $ liftA3 BinOp parseBop parseExpr parseExpr

parseTernOp :: Parser Expr
parseTernOp = (optionalModifier paren . trim)
            $ TernOp <$> parseTop <*> parseExpr <*> parseExpr <*> parseExpr

parseGuard :: Parser Expr
parseGuard = Guard
          <$> (oneOrMore $ liftA2 (,) (trim $ char '?' *> parseExpr) (trimLeft parseExpr))

parseExpr :: Parser Expr
parseExpr =  trimLeft
          $  parseCall
         <|> parseBinOp
         <|> parseTernOp
         <|> parseGuard
         <|> (Number <$> number)
         <|> (Ident <$> camel)
