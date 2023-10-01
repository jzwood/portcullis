{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module CodeGen.Html.Target where

import Prelude hiding (showList)
import Data.Functor
import Data.Bifunctor
import Data.Function
import Control.Applicative
import Data.Char
import Data.List (intercalate, intersperse, nub, dropWhileEnd, groupBy)
import Data.Map (Map, (!))
import CodeGen.Po.Target (toPo)
import qualified Data.Map as Map

import Syntax
import Util

class Html ast where
  toHtml :: ast -> String

escapeHtml :: String -> String
escapeHtml content = content >>= (\case '&' -> "&amp;"; '<' -> "&lt;"; '>' -> "&gt;"; char -> [char])

tag :: String -> String -> String -> String
tag element className content = concat [ "<", element, " class=\"", className, "\"", ">"
                                       , escapeHtml content
                                       , "</", element, ">"
                                       ]

instance Html Module where
  toHtml Module { stmts } = unlines' $ toHtml <$> stmts

instance Html Stmt where
  toHtml (F f) = toHtml f
  toHtml (S s) = toHtml s
  toHtml (P p) = toHtml p
  toHtml (C c) = toHtml c

instance Html Function where
  toHtml (Function name tExpr vars expr)
    = tag "code" "function" $ unlines' [ unwords [ name, toHtml tExpr]
               , unwords [name, unwords vars, "="]
               , indent $ toHtml expr
               ]

instance Html Pipe where
  toHtml Pipe { funcName, inStreams, outStreamName } =
    unwords ["|", funcName, (bracket . pad. unwords) $ (\(a, b) -> unwords [a, show b]) <$> inStreams, outStreamName]

instance Html Stream where
  toHtml Stream { streamName, streamSig } = unwords [ streamName, toHtml streamSig ]

instance Html Comment where
  toHtml (Comment comment) = concat ["#", comment, "#"]

instance Html TypeExpr where
  toHtml NumType = "Num"
  toHtml ByteType = "Byte"
  toHtml AtomType = "Atom"
  toHtml (Unspecified t) = extractExt t
  toHtml (ListType t)
    = toHtml t
    & bracket
  toHtml (TupType t1 t2)
    =  [t1, t2]
   <&> toHtml
    &  curly . unwords
  toHtml (Arrow tExpr1 tExpr2) = unwords ["->", toHtml tExpr1, toHtml tExpr2]

instance Html Value where
  toHtml (Number n) = if fromInteger (round n) == n then show $ fromInteger $ round n else show n
  toHtml (Byte b) = show b
  toHtml (Atom n) = n
  toHtml (List t xs) = concat [ toHtml t , ":" , (bracket . unwords) $ toHtml <$> xs ]
  toHtml (Tuple e1 e2)
    = toHtml <$> [e1, e2]
    & curly . unwords

instance Html Expr where
  toHtml (Val p) = toHtml p
  toHtml (Ident name) = name
  toHtml (Call name exprs) = (paren . unwords . filter (not . null)) [ name, unwords $ toHtml <$> exprs]
  toHtml (UnOp unop e) = unwords [ toHtml unop, toHtml e]
  toHtml (BinOp bop e1 e2) = unwords [toHtml bop, toHtml e1, toHtml e2]
  toHtml (TernOp If p e1 e2)
    = unlines' [ unwords ["?", toHtml p] , indent $ toHtml e1 , toHtml e2 ]
  toHtml (TernOp Uncons xs b fb)
    = unwords [ "<+", toHtml xs, toHtml b, toHtml fb ]

instance Html UnOp where
  toHtml Fst = "@1"
  toHtml Snd = "@2"

instance Html Bop where
  toHtml Plus = "+"
  toHtml Minus = "-"
  toHtml Times = "*"
  toHtml Divide = "/"
  toHtml GreaterThan = ">"
  toHtml GreaterThanOrEqual = ">="
  toHtml LessThan = "<"
  toHtml LessThanOrEqual = "<="
  toHtml Rem = "%"
  toHtml Equal = "=="
  toHtml Cons = "+>"
