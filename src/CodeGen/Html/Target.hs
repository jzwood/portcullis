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

esc :: String -> String
esc content = content >>= (\case '&' -> "&amp;"; '<' -> "&lt;"; '>' -> "&gt;"; char -> [char])

tag :: String -> String -> String -> String
tag element className content = concat [ "<", element, " class=\"", className, "\"", ">"
                                       , content
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
    = tag "code" "function" $ unlines' [ unwords [ name, tag "span" "type-expr" (toHtml tExpr)]
               , unwords [name, unwords vars, "="]
               , indent $ toHtml expr
               ]

instance Html Pipe where
  toHtml pipe = tag "span" "pipe" (toPo pipe)

instance Html Stream where
  toHtml Stream { streamName, streamSig } = tag "code" "stream" $ unwords [ streamName, toHtml streamSig ]

instance Html Comment where
  toHtml comment@(Comment _) = tag "p" "comment" $ esc . toPo $ comment

instance Html TypeExpr where
  toHtml te = esc . toPo $ te

instance Html Value where
  toHtml num@(Number _) = tag "span" "num" (toPo num)
  toHtml byte@(Byte _) = tag "span" "byte" (toPo byte)
  toHtml atom@(Atom _) = tag "span" "atom" (toPo atom)
  toHtml (List t xs) = concat [ toHtml t , ":" , (bracket . unwords) $ toHtml <$> xs ]
  toHtml (Tuple e1 e2)
    = toHtml <$> [e1, e2]
    & curly . unwords

instance Html Expr where
  toHtml (Val p) = toHtml p
  toHtml (Ident name) = tag "var" "ident" name
  toHtml (Call name exprs) = tag "span" "call" $ (paren . unwords . filter (not . null)) [ name, unwords $ toHtml <$> exprs]
  toHtml (UnOp unop e) = unwords [ toHtml unop, toHtml e]
  toHtml (BinOp bop e1 e2) = unwords [toHtml bop, toHtml e1, toHtml e2]
  toHtml (TernOp If p e1 e2)
    = unlines' [ unwords [tag "span" "op" "?", toHtml p] , indent $ toHtml e1 , toHtml e2 ]
  toHtml (TernOp Uncons xs b fb)
    = unwords [ tag "span" "op" (esc "<+"), toHtml xs, toHtml b, toHtml fb ]

instance Html UnOp where
  toHtml op = tag "span" "op" (toPo op)

instance Html Bop where
  toHtml op = tag "span" "op" (esc . toPo $ op)
