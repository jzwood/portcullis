module Parser where

import MiniParser
import Syntax
import CodeGen
import Data.Functor
import Control.Applicative
import Data.Char
import Data.List
import Util hiding (paren)

parseProgram :: Parser Module
parseProgram = parseModule <* spaces

parseModule :: Parser Module
parseModule =  Module <$> oneOrMore parseStmt

parseQueue :: Parser Queue
parseQueue = Queue <$> address <*> integer <*> parseTypeExpr

parsePipe :: Parser Pipe
parsePipe = Pipe <$> (char '|' *> trim camel) <*> (brack . trim $ zeroOrMore address) <*> address

parseComment :: Parser Comment
parseComment =  Comment
            <$> (char '#' *> oneOrMore (satisfy (/= '#')) <* char '#')

parseFunc :: Parser Func
parseFunc =  trimLeft
          $  Function
         <$> name
         <*> parseTypeExpr
         <*  name
         <*> zeroOrMore name
         <*  trimLeft (char '=')
         <*> parseExpr
         where name = trimLeft camel

parseStmt :: Parser Stmt
parseStmt =  trimLeft
          $  F <$> parseFunc
         <|> Q <$> parseQueue
         <|> P <$> parsePipe
         <|> C <$> parseComment

parseArrow :: Parser TypeExpr
parseArrow = optionalParens $ liftA2 Arrow (word "->" *> parseTypeExpr) parseTypeExpr

parseTupType :: Parser TypeExpr
parseTupType = liftA2 TupType (char '{' *> parseTypeExpr) (parseTypeExpr <* spaces <* char '}')

parseTypeExpr :: Parser TypeExpr
parseTypeExpr
   =  trimLeft
   $  NumType <$ word (show NumType)
  <|> CharType <$ word (show CharType)
  <|> AtomType <$ word (show AtomType)
  <|> Unspecfied <$> camel
  <|> ListType <$> brack (trim parseTypeExpr)
  <|> parseTupType
  <|> parseArrow

parseUnOp :: Parser UnOp
parseUnOp =  word "@1" $> Fst
         <|> word "@2" $> Snd

parseBop :: Parser Bop
parseBop = word "==" $> Equal
       <|> word "+>" $> Cons
       <|> word ">=" $> GreaterThanOrEqual
       <|> word "<=" $> LessThanOrEqual
       <|> char '>' $> GreaterThan
       <|> char '<' $> LessThan
       <|> char '+' $> Plus
       <|> char '-' $> Minus
       <|> char '*' $> Times
       <|> char '/' $> Divide
       <|> char '%' $> Rem

parseTop :: Parser Top
parseTop =  word "<+" $> Uncons
        <|> char '?' $> If

parseCall :: Parser Expr
parseCall = paren . trim
          $ liftA2 Call camel (zeroOrMore parseExpr)

parseUnaryOp :: Parser Expr
parseUnaryOp = optionalParens $ liftA2 UnOp parseUnOp parseExpr

parseBinOp :: Parser Expr
parseBinOp = optionalParens $ liftA3 BinOp parseBop parseExpr parseExpr

parseTernOp :: Parser Expr
parseTernOp = optionalParens $ TernOp <$> parseTop <*> parseExpr <*> parseExpr <*> parseExpr

parseChar :: Parser Char
parseChar = wrap '\'' '\'' anyChar

parseList :: Parser Value
parseList = List <$> parseTypeExpr <*> trimLeft (brack $ trim $ zeroOrMore parseExpr)

parseTuple :: Parser Value
parseTuple = liftA2 Tuple (char '{' *> parseExpr) (parseExpr <* spaces <* char '}')

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
          $  parseUnaryOp
         <|> parseCall
         <|> parseTernOp
         <|> parseBinOp
         <|> Val <$> parseValue
         <|> Ident  <$> camel
