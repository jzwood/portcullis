module Parser where

import MiniParser
import Syntax
import Data.Functor
import Control.Applicative
import Data.Char
import Data.List
import Util hiding (paren)

parseModule :: Parser [Stmt]
parseModule = oneOrMore parseStmt

parseStmt :: Parser Stmt
parseStmt =  trimLeft
          $  parseFunc
         <|> parseSignature

parseFunc :: Parser Stmt
parseFunc = Function
         <$> camel
         <*> trimLeft (zeroOrMore $ Var <$> (trimLeft camel))
         <*  trimLeft (char '=')
         <*> trimLeft parseExpr

parseSignature :: Parser Stmt
parseSignature = liftA2 Signature camel parseTypeExpr

parseArrow :: Parser TypeExpr
parseArrow = liftA2 Arrow (word "->" *> parseTypeExpr) parseTypeExpr

parseTupType :: Parser TypeExpr
parseTupType = liftA2 TupType (char '[' *> parseTypeExpr) (parseTypeExpr <* spaces <* char ']')

parseTypeExpr :: Parser TypeExpr
parseTypeExpr
   =  trimLeft
   $  NumType <$ (word $ show NumType)
  <|> CharType <$ (word $ show CharType)
  <|> AtomType <$ (word $ show AtomType)
  <|> Unspecfied <$> camel
  <|> ListType <$> (brack $ trim parseTypeExpr)
  <|> parseTupType
  <|> parseArrow

parseUnOp :: Parser UnOp
parseUnOp =  word "fst" $> Fst
         <|> word "snd" $> Snd

parseBop :: Parser Bop
parseBop = word "==" $> Equal
       <|> word "/=" $> NotEqual
       <|> word "++" $> Concat
       <|> char '>' $> GreaterThan
       <|> char '<' $> LessThan
       <|> char '+' $> Plus
       <|> char '-' $> Minus
       <|> char '*' $> Times
       <|> char '/' $> Divide
       <|> char '%' $> Rem

parseTop :: Parser Top
parseTop =  word "slice" $> Slice
        <|> char '!' $> Get

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
          <$> (oneOrMore $ liftA2 (,) (trim $ char '?' *> parseExpr) parseExpr)

parseChar :: Parser Char
parseChar = wrap '\'' '\'' anyChar

parseList :: Parser Value
parseList = List <$> parseTypeExpr <*> trimLeft (brack $ trim $ zeroOrMore parseExpr)

parseTuple :: Parser Value
parseTuple = liftA2 Tuple (char '[' *> parseExpr) (parseExpr <* spaces <* char ']')

parseValue :: Parser Value
parseValue
  =  trimLeft
  $  Number <$> number
 <|> Character <$> parseChar
 <|> parseList
 <|> Atom <$> pascal
 <|> parseTuple

parseExpr :: Parser Expr
parseExpr =  trimLeft
          $  parseCall
         <|> parseBinOp
         <|> parseTernOp
         <|> parseGuard
         <|> Val <$> parseValue
         <|> Ident  <$> camel
