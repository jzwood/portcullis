{-# LANGUAGE NamedFieldPuns #-}

module Parser where

import MiniParser
import Syntax
import CodeGen
import Data.Functor
import Control.Applicative
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Util hiding (paren)

parseProgram :: Parser Module
parseProgram = parseModule <* spaces

alphaConversion :: String -> TypeExpr -> TypeExpr
alphaConversion id (Unspecfied name) = Unspecfied (id ++ "." ++ name)
alphaConversion id (TupType t1 t2) = TupType (alphaConversion id t1) (alphaConversion id t2)
alphaConversion id (ListType t) =  ListType (alphaConversion id t)
alphaConversion id (Arrow tl tr) = Arrow (alphaConversion id tl) (alphaConversion id tr)
alphaConversion id t = t

alphaConversion' :: Name -> TypeExpr -> TypeExpr
alphaConversion' fname (Unspecfied name) = Unspecfied (fname ++ "." ++ name)
alphaConversion' fname t = applyTypeExpr (alphaConversion' fname) t

moduleAlg :: Stmt -> Module -> Module
moduleAlg (F func@Function { name = fname , signature }) mod@Module { functions, functionMap } =
  mod { functions = function : functions, functionMap = Map.insert (name function) function functionMap }
    where function = func { signature = alphaConversion fname signature }
moduleAlg (C comment) mod@Module { comments } = mod { comments = comment : comments}
moduleAlg (Q queue@Queue { queueName }) mod@Module{ queues, queueMap } = mod { queues = queue : queues, queueMap = Map.insert queueName queue queueMap}
moduleAlg (P pipe) mod@Module{ pipes } = mod { pipes = pipe : pipes}

stmtsToModule:: [Stmt] -> Module
stmtsToModule = foldr moduleAlg (Module { functions = [], functionMap = Map.empty, comments = [], queues = [], queueMap = Map.empty, pipes = [] })

parseModule :: Parser Module
parseModule =  stmtsToModule <$> oneOrMore parseStmt

parseQueue :: Parser Queue
parseQueue = Queue <$> address <*> trimLeft integer <*> parseTypeExpr

parsePipe :: Parser Pipe
parsePipe = Pipe <$> (char '|' *> trimLeft camel) <*> trim (brack . trim $ zeroOrMore (trim address)) <*> address

parseComment :: Parser Comment
parseComment =  Comment
            <$> (char '#' *> oneOrMore (satisfy (/= '#')) <* char '#')

parseFunc :: Parser Function
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
parseList =  List <$> parseTypeExpr <*> trimLeft (brack $ trim $ zeroOrMore parseExpr)

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
