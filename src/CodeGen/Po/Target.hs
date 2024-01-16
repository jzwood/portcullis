{-# LANGUAGE NamedFieldPuns #-}

module CodeGen.Po.Target where

import Prelude hiding (showList)
import Data.Functor
import Data.Bifunctor
import Data.Function
import Control.Applicative
import Data.Char
import Data.List (intercalate, intersperse, nub, dropWhileEnd, groupBy)
import Data.Map (Map, (!))
import qualified Data.Map as Map

import Syntax
import Utils

class Po ast where
  toPo :: ast -> String

groupStmt :: Stmt -> Stmt -> Bool
groupStmt (S _) (S _) = True
groupStmt (P _) (P _) = True
groupStmt (C _) (C _) = True
groupStmt _ _ = False

instance Po Module where
  toPo Module { stmts }
    =   groupBy groupStmt stmts
   <&>  (unlines . fmap toPo)
    &   unlines'

instance Po Stmt where
  toPo (F f) = toPo f
  toPo (S s) = toPo s
  toPo (P p) = toPo p
  toPo (C c) = toPo c

instance Po Function where
  toPo (Function name tExpr vars expr)
    = unlines' [ unwords [ name, toPo tExpr]
               , unwords [name, unwords vars, "="]
               , indent $ toPo expr
               ]

instance Po Pipe where
  toPo Pipe { funcName, inStreams, outStreamName } =
    unwords ["|", funcName, (bracket . pad. unwords) $ (\(a, b) -> unwords [a, show b]) <$> inStreams, outStreamName]

instance Po Stream where
  toPo Stream { streamName, streamSig } = unwords [ streamName, toPo streamSig ]

instance Po Comment where
  toPo (Comment comment) = concat ["#", comment, "#"]

instance Po TypeExpr where
  toPo NumType = "Num"
  toPo ByteType = "Byte"
  toPo AtomType = "Atom"
  toPo (Unspecified t) = extractExt t
  toPo (ListType t)
    = toPo t
    & bracket
  toPo (TupType t1 t2)
    =  [t1, t2]
   <&> toPo
    &  curly . unwords
  toPo (Arrow tExpr1 tExpr2) = unwords ["->", toPo tExpr1, toPo tExpr2]

instance Po Value where
  toPo (Number n) = if fromInteger (round n) == n then show $ fromInteger $ round n else show n
  toPo (Byte b) = show b
  toPo (Atom n) = n
  toPo (List t xs) = concat [ toPo t , ":" , (bracket . unwords) $ toPo <$> xs ]
  toPo (Tuple e1 e2)
    = toPo <$> [e1, e2]
    & curly . unwords

instance Po Expr where
  toPo (Val p) = toPo p
  toPo (Ident name) = name
  toPo (Call name exprs) = (paren . unwords') [ name, unwords $ toPo <$> exprs]
  toPo (UnOp unop e) = unwords [ toPo unop, toPo e]
  toPo (BinOp bop e1 e2) = unwords [toPo bop, toPo e1, toPo e2]
  toPo (TernOp If p e1 e2)
    = unlines' [ unwords ["?", toPo p] , indent $ toPo e1 , toPo e2 ]
  toPo (TernOp Uncons xs b fb)
    = unwords [ "<+", toPo xs, toPo b, toPo fb ]

instance Po UnOp where
  toPo Fst = "@1"
  toPo Snd = "@2"

instance Po Bop where
  toPo Plus = "+"
  toPo Minus = "-"
  toPo Times = "*"
  toPo Divide = "/"
  toPo GreaterThan = ">"
  toPo GreaterThanOrEqual = ">="
  toPo LessThan = "<"
  toPo LessThanOrEqual = "<="
  toPo Rem = "%"
  toPo Equal = "=="
  toPo Cons = "+>"
