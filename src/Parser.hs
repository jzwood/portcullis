{-# LANGUAGE NamedFieldPuns #-}

module Parser where

import MiniParser
import Syntax
import Data.Functor
import Control.Applicative
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Util hiding (paren)

parseProgram :: Parser Module
parseProgram = parseModule <* spaces

alphaConversion :: Name -> TypeExpr -> TypeExpr
alphaConversion fname (Unspecified name) = Unspecified (fname ++ "." ++ name)
alphaConversion fname t = applyTypeExpr (alphaConversion fname) t

alphaConvertExpr :: Name -> Expr -> Expr
alphaConvertExpr _ ident@(Ident _) = ident
alphaConvertExpr fname (Val val) =
  case val of
    Tuple e1 e2 -> Val $ Tuple (alphaConvertExpr fname e1) (alphaConvertExpr fname e2)
    List te es -> Val $ List (alphaConversion fname te) (fmap (alphaConvertExpr fname) es)
    _val -> Val val
alphaConvertExpr fname e = applyExpr (alphaConvertExpr fname) e

alphaConvertFunction :: Function -> Function
alphaConvertFunction func@Function { name, signature, body }
  = func { signature = alphaConversion name signature, body = alphaConvertExpr name body }

stmtAlg :: Stmt -> Module -> Module
stmtAlg stmt mod@Module{ stmts } = mod { stmts = stmt : stmts }

moduleAlg :: Stmt -> Module -> Module
moduleAlg (F func) mod@Module { functions, functionMap }
  = mod { functions = function : functions, functionMap = Map.insert (name function) function functionMap }
    where function = alphaConvertFunction func
moduleAlg (C comment) mod@Module { comments } = mod { comments = comment : comments}
moduleAlg (S stream@Stream { streamName }) mod@Module{ streams, streamMap }
  = mod { streams = stream : streams, streamMap = Map.insert streamName stream streamMap}
moduleAlg (P pipe) mod@Module{ pipes } = mod { pipes = pipe : pipes}

stmtsToModule:: [Stmt] -> Module
stmtsToModule = foldr (\stmt mod -> moduleAlg stmt (stmtAlg stmt mod)) (Module { stmts = [], functions = [], functionMap = Map.empty, comments = [], streams = [], streamMap = Map.empty, pipes = [] })

parseModule :: Parser Module
parseModule =  stmtsToModule <$> oneOrMore parseStmt

parseStream :: Parser Stream
parseStream = Stream <$> stream <*> parseTypeExpr

parsePipe :: Parser Pipe
parsePipe = Pipe <$> (char '|' *> trimLeft camel) <*> trim (brack . trim $ zeroOrMore (liftA2 (,) (trimLeft stream) (trimLeft integer))) <*> stream

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
         <|> S <$> parseStream
         <|> P <$> parsePipe
         <|> C <$> parseComment

parseArrow :: Parser TypeExpr
parseArrow = optionalParens $ liftA2 Arrow (word "->" *> parseTypeExpr) parseTypeExpr

parseTupType :: Parser TypeExpr
parseTupType = liftA2 TupType (char '{' *> parseTypeExpr) (parseTypeExpr <* spaces <* char '}')

parseTypeExpr :: Parser TypeExpr
parseTypeExpr
   =  trimLeft
   $  NumType <$ word "Num"
  <|> ByteType <$ word "Byte"
  <|> AtomType <$ word "Atom"
  <|> Unspecified <$> camel
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

parseByte :: Parser Integer
parseByte = integer >>= byte

parseList :: Parser Value
parseList =  List <$> parseTypeExpr <*> (trim (char ':') *> (brack . trim $ zeroOrMore parseExpr))

parseTuple :: Parser Value
parseTuple = liftA2 Tuple (char '{' *> parseExpr) (parseExpr <* spaces <* char '}')

parseValue :: Parser Value
parseValue
  =  trimLeft
  $  Number <$> number
 <|> Byte <$> parseByte
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
