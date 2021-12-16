module Parser where

import MiniParser
import Syntax
import Data.Functor
import Control.Applicative
import Data.Char
import Data.List
import Util hiding (paren)

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
   $  (NumType <$ (word $ show NumType))
  <|> (CharType <$ (word $ show CharType))
  <|> (AtomType <$ (word $ show AtomType))
  <|> (ListType <$> (brack '[' ']' $ trim parseTypeExpr))
  <|> (Unspecfied <$> camel)
  <|> parseArrow

parseBop :: Parser Bop
parseBop = (word "==" $> Equal)
       <|> (word "/=" $> NotEqual)
       <|> (word "++" $> Concat)
       <|> (char '>' $> GreaterThan)
       <|> (char '<' $> LessThan)
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

parseChar :: Parser Char
parseChar = char '\'' *> anyChar <* char '\''

parseValue :: Parser Value
parseValue =  (Number <$> number)
               <|> (Character <$> parseChar)
               <|> (Atom <$> pascal)

parseExpr :: Parser Expr
parseExpr =  trimLeft
          $  parseCall
         <|> parseBinOp
         <|> parseTernOp
         <|> parseGuard
         <|> Val <$> parseValue
         <|> Ident <$> camel
